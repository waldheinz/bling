
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Bling.Image (

   Image, ImageSample, mkImage, rgbPixels, imageWindow', imgW, imgH,
   getPixel,

   MImage,

   mkMImage, mkImageTile, addSample, splatSample, addTile, splitMImage,
   scaleSplats,

   imageWidth, imageHeight, sampleExtent,

   thaw, freeze,

   -- * The Image Transformer
   ImageT, runImageT, evalImageT, execImageT,
   addSample', sampleExtent', addUnfilteredSample'
   ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Debug.Trace

import           Graphics.Bling.Filter
import           Graphics.Bling.Sampling
import           Graphics.Bling.Spectrum
import           Graphics.Bling.Types

-- | size of the precomputed pixel filter table
filterTableSize :: Int
filterTableSize = 16

data TableFilter = TableFilter
   { tblFltSize   :: {-# UNPACK #-} ! (Float, Float)
   , tblVals      :: ! (V.Vector Float)
   }

mkTableFilter :: Filter -> TableFilter
mkTableFilter flt = TableFilter (filterSize flt) $ runST $ do
   v <- MV.new (filterTableSize * filterTableSize)
   let (fw, fh) = filterSize flt

   forM_ [0 .. (filterTableSize - 1)] $ \y -> do
      let fy = (fromIntegral y + 0.5) * fh / fromIntegral filterTableSize

      forM_ [0 .. (filterTableSize - 1)] $ \x -> do
         let fx = (fromIntegral x + 0.5) * fw / fromIntegral filterTableSize

         MV.write v (y * filterTableSize + x) $ evalFilter flt fx fy

   V.freeze v

-- | a mutable image
data MImage m = MImage
   { imageWidth      :: {-# UNPACK #-} ! Int
   , imageHeight     :: {-# UNPACK #-} ! Int
   , _imageOffset    :: {-# UNPACK #-} ! (Int, Int)
   , _imageFilter    :: {-# UNPACK #-} ! TableFilter
   , _imagePixels    :: ! (MV.MVector (PrimState m) Float)
   , _mSplatPixels   :: ! (MV.MVector (PrimState m) Float)
   }

-- | creates a new image where all pixels are initialized to black
mkMImage :: (PrimMonad m)
   => Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> m (MImage m)
mkMImage flt w h = do
   p <- MV.replicate (w * h * 4) 0
   s <- MV.replicate (w * h * 3) 0
   return $! MImage w h (0, 0) (mkTableFilter flt) p s

splitMImage :: (PrimMonad m)
   => MImage m1
   -> m (MImage m)
{-# INLINE splitMImage #-}
splitMImage (MImage w h o f px spx) = do
   p <- MV.replicate (MV.length px) 0
   s <- MV.replicate (MV.length spx) 0
   return $! MImage w h o f p s

scaleSplats :: (PrimMonad m)
   => MImage m
   -> (Int -> m Float)
   -> m ()
{-# INLINE scaleSplats #-}
scaleSplats (MImage w h _ _ _ spx) sf = do
   forM_ [0 .. (w * h -1)] $ \i -> do
      ox <- MV.unsafeRead spx (i * 3 + 0)
      oy <- MV.unsafeRead spx (i * 3 + 1)
      oz <- MV.unsafeRead spx (i * 3 + 2)
      s <- sf i
      MV.unsafeWrite spx (i * 3 + 0) (ox * s)
      MV.unsafeWrite spx (i * 3 + 1) (oy * s)
      MV.unsafeWrite spx (i * 3 + 2) (oz * s)

mkImageTile :: (PrimMonad m) => MImage m1 -> SampleWindow -> m (MImage m)
{-# INLINE mkImageTile #-}
mkImageTile img wnd = do
   let
      w = xEnd wnd - px + floor (0.5 + fw)
      h = yEnd wnd - py + floor (0.5 + fh)
      px = max 0 $ xStart wnd -- + floor (0.5 - fw)
      py = max 0 $ yStart wnd -- + floor (0.5 - fh)
      (fw, fh) = tblFltSize $ _imageFilter img

   p <- MV.replicate (w * h * 4) 0
   s <- MV.replicate (w * h * 3) 0
   return $! MImage w h (px, py) (_imageFilter img) p s

-- | an immutable image
data Image = Img
   { imgW         :: {-# UNPACK #-} ! Int
   , imgH         :: {-# UNPACK #-} ! Int
   , _imgFilter   :: {-# UNPACK #-} ! TableFilter
   , _imgP        :: ! (V.Vector Float)
   , _imgS        :: ! (V.Vector Float)
   }

instance NFData Image where
   rnf (Img w h f p s) = w `seq` h `seq` f `seq` p `seq` s `seq ` ()

instance Show Image where
   show (Img w h _ _ _) = "Image {width=" ++ (show w) ++ ", height=" ++ (show h) ++ "}"

-- | creates a new image where all pixels are initialized to black
mkImage
   :: Filter      -- ^ the pixel filter function to use when adding samples
   -> PixelSize   -- ^ the image width
   -> Image
mkImage flt (w, h) = Img w h (mkTableFilter flt) p s where
   p = V.replicate (w * h * 4) 0
   s = V.replicate (w * h * 3) 0

-- | converts an image to a mutable image
thaw :: (PrimMonad m) => Image -> m (MImage m)
{-# INLINE thaw #-}
thaw (Img w h f p s) = {-# SCC "thaw" #-} do
   p' <- GV.thaw p
   s' <- GV.thaw s
   return $! MImage w h (0, 0) f p' s'

-- | converts a mutable image to an image (and the offsets)
freeze :: (PrimMonad m) => MImage m -> m (Image, (Int, Int))
{-# INLINE freeze #-}
freeze (MImage w h o f p s) = {-# SCC "freeze" #-} do
   p' <- GV.freeze p
   s' <- GV.freeze s
   return $! (Img w h f p' s', o)

sampleExtent :: MImage m -> SampleWindow
sampleExtent (MImage w h (ox, oy) f _ _) = SampleWindow x0 x1 y0 y1 where
   x0 = ox + floor (0.5 - fw)
   x1 = ox + floor (0.5 + (fromIntegral w) + fw)
   y0 = oy + floor (0.5 - fh)
   y1 = oy + floor (0.5 + (fromIntegral h) + fh)
   (fw, fh) = tblFltSize f

imageWindow' :: Image -> SampleWindow
imageWindow' (Img w h flt _ _) = SampleWindow x0 x1 y0 y1 where
   x0 = floor (0.5 - fw)
   x1 = floor (0.5 + (fromIntegral w) + fw)
   y0 = floor (0.5 - fh)
   y1 = floor (0.5 + (fromIntegral h) + fh)
   (fw, fh) = tblFltSize flt

addTile :: PrimMonad m => MImage m -> (Image, (Int, Int)) -> m ()
{-# INLINE addTile #-}
addTile (MImage w h (ox, oy) _ px ps) (Img tw th _ px' ps', (dx, dy)) = {-# SCC addTile #-} do
   forM_ [(x, y) | y <- [0 .. (th-1)], x <- [0 .. (tw-1)]] $ \(x, y) -> do
      let
         od    = w * (y - oy + dy) + (x - ox + dx)
         od'   = 4 * od
         ods'  = 3 * od
         os    = tw * y + x
         os'   = 4 * os
         oss'  = 3 * os

      unless ((y - oy + dy) >= h || (x - ox + dx) >= w) $ do
         -- the splats
         forM_ [0..2] $ \o -> do
            old <- MV.unsafeRead ps (ods' + o)
            MV.unsafeWrite ps (ods' + o) (old + V.unsafeIndex ps' (oss' + o))

         -- the pixels
         forM_ [0..3] $ \o -> do
            old <- MV.unsafeRead px (od' + o)
            MV.unsafeWrite px (od' + o) (old + V.unsafeIndex px' (os' + o))

splatSample :: PrimMonad m => MImage m -> Float -> Float -> Spectrum -> m ()
{-# INLINE splatSample #-}
splatSample !(MImage !w !h (!iox, !ioy) _ _ !p) !sx !sy !ss
   | px >= w || py >= h || px < 0 || py < 0 = return ()
   | sNaN ss = trace ("not splatting NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("not splatting infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "splatSample" #-} do
      ox <- MV.unsafeRead p $ o + 0
      oy <- MV.unsafeRead p $ o + 1
      oz <- MV.unsafeRead p $ o + 2

      MV.unsafeWrite p (o + 0) $ ox + dx
      MV.unsafeWrite p (o + 1) $ oy + dy
      MV.unsafeWrite p (o + 2) $ oz + dz
      where
         px = floor sx - iox
         py = floor sy - ioy
         o = 3 * (px + py * w)
         (dx, dy, dz) = spectrumToXYZ ss

addUnfilteredSample :: PrimMonad m => MImage m -> Float -> Float -> Spectrum -> m ()
addUnfilteredSample !(MImage !w !h (!iox, !ioy) _ p _) !sx !sy !ss
   | px >= w || py >= h || px < 0 || py < 0 = return ()
   | sNaN ss = trace ("ignore unfiltered NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("ignore unfiltered sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = do

      ow <- MV.unsafeRead p $ o + 0
      ox <- MV.unsafeRead p $ o + 1
      oy <- MV.unsafeRead p $ o + 2
      oz <- MV.unsafeRead p $ o + 3

      MV.unsafeWrite p (o + 0) $ ow + 1
      MV.unsafeWrite p (o + 1) $ ox + dx
      MV.unsafeWrite p (o + 2) $ oy + dy
      MV.unsafeWrite p (o + 3) $ oz + dz

      where
         px = floor sx - iox
         py = floor sy - ioy
         o = 4 * (px + py * w)
         (dx, dy, dz) = spectrumToXYZ ss


-- | adds a sample to the specified image
addSample :: PrimMonad m => MImage m -> Float -> Float -> Spectrum -> m ()
{-# INLINE addSample #-}
addSample (MImage !w !h (!ox, !oy) !ftbl !p _) !sx !sy !ss
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "addSample" #-} do

      let
         (smx, smy, smz) = spectrumToXYZ ss
         (fw, fh) = let (a, b) = tblFltSize ftbl in (a, b)
         ifw = 1 / fw
         ifh = 1 / fw

         dx = sx - 0.5
         dy = sy - 0.5

         x0 = max ox (ceiling $ dx - fw)
         x1 = min (ox + w - 1) (floor $ dx + fw)
         y0 = max oy (ceiling $ dy - fh)
         y1 = min (oy + h - 1) (floor $ dy + fh)

      unless ((x1 - x0) < 0 || (y1 - y0) < 0) $ do
         let
            ifx = V.generate (x1 - x0 + 1) $
               \x -> let x' = x + x0
                     in let fx = abs $ (fromIntegral x' - dx) * ifw * fromIntegral filterTableSize
                     in min (floor fx) (filterTableSize - 1)

            ify = V.generate (y1 - y0 + 1) $
               \y -> let y' = y + y0
                     in let fy = abs $ (fromIntegral y' - dy) * ifh * fromIntegral filterTableSize
                     in min (floor fy) (filterTableSize - 1)

         forM_ [(x, y) | y <- [y0..y1], x <- [x0..x1]] $ \(x, y) -> do
            let
               imgo = 4 * ((x - ox) + (y - oy) * w)
               flto = V.unsafeIndex ify (y - y0) * filterTableSize + V.unsafeIndex ifx (x-x0)
               fltw = V.unsafeIndex (tblVals ftbl) flto

            orw <- MV.unsafeRead p imgo
            orx <- MV.unsafeRead p (imgo + 1)
            ory <- MV.unsafeRead p (imgo + 2)
            orz <- MV.unsafeRead p (imgo + 3)

            MV.unsafeWrite p imgo       $ orw + fltw
            MV.unsafeWrite p (imgo + 1) $ (orx + smx * fltw)
            MV.unsafeWrite p (imgo + 2) $ (ory + smy * fltw)
            MV.unsafeWrite p (imgo + 3) $ (orz + smz * fltw)

-- | extracts the pixel at the specified offset from an Image
getPixel
   :: Image
   -> Float -- ^ splat weight
   -> Int
   -> (Float, Float, Float) -- ^ (r, g, b)
getPixel (Img _ _ _ p s) sw o
  | w == 0    = xyzToRgb (sw * sr, sw * sg, sw * sb)
  | otherwise = xyzToRgb (sw * sr + r * w', sw * sg + g * w', sw * sb + b * w')
  where
    w'           = 1 / w
    o'           = 4 * o
    (w, r, g, b) = (p V.! o', p V.! (o' + 1), p V.! (o' + 2), p V.! (o' + 3))
    os'          = 3 * o
    (sr, sg, sb) = (s V.! os', s V.! (os' + 1), s V.! (os' + 2))

-- | applies gamma correction to an RGB triple
gamma :: Float -> (Float, Float, Float) -> (Float, Float, Float)
gamma x (r, g, b) = let x' = 1 / x in (r ** x', g ** x', b ** x')

-- | converts a Float in [0..1] to an Int in [0..255], clamping values outside [0..1]
clamp :: Float -> Int
clamp v = round ( min 1 (max 0 v) * 255 )

rgbPixels :: Image -> Float -> SampleWindow -> [((Int, Int), (Int, Int, Int))]
rgbPixels img@(Img w h _ _ _) spw wnd = Prelude.zip xs clamped where
   ps = map (getPixel img spw) os
   rgbs = map (gamma 2.2) ps
   clamped = map (\(r,g,b) -> (clamp r, clamp g, clamp b)) rgbs
   xs = filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) $ coverWindow wnd
   os = map (\(x,y) -> (y * (imgW img)) + x) xs

newtype ImageT m a = ImageT { withImage :: MImage m -> m a }

--instance Functor m => Functor (ImageT m) where
--    fmap f (ImageT c) = ImageT $ fmap f c

instance Functor m => Functor (ImageT m) where
    fmap k (ImageT c1) = ImageT $ \i -> fmap k (c1 i)

instance Applicative m => Applicative (ImageT m) where
    pure x = ImageT $ \_ -> pure x
    (<*>) (ImageT k) (ImageT c) = ImageT $ \i -> (k i) <*> (c i)

instance Monad m => Monad (ImageT m) where
  return x = ImageT $ \_ -> return x
  (>>=) (ImageT c1) fc2 = ImageT $ \img -> c1 img >>= \a -> withImage (fc2 a) img

runImageT :: PrimMonad m => ImageT m a -> Image -> m (a, Image)
runImageT k img = do
  mimg      <- thaw img
  result    <- withImage k mimg
  (img', _) <- freeze mimg
  img' `seq` return (result, img')

execImageT :: PrimMonad m => ImageT m a -> Image -> m Image
execImageT k i = liftM snd $ runImageT k i

evalImageT :: PrimMonad m => ImageT m a -> Image -> m a
evalImageT k i = liftM fst $ runImageT k i

addSample' :: PrimMonad m => Float -> Float -> Spectrum -> ImageT m ()
addSample' x y s = ImageT $ \img -> addSample img x y s

addUnfilteredSample' :: PrimMonad m => Float -> Float -> Spectrum -> ImageT m ()
addUnfilteredSample' x y s = ImageT $ \img -> addUnfilteredSample img x y s


sampleExtent' :: Monad m => ImageT m SampleWindow
sampleExtent' = ImageT $ \i -> return $ sampleExtent i
