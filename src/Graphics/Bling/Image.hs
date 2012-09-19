
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Graphics.Bling.Image (
   
   Image, ImageSample, mkImage, rgbPixels, imageWindow', imgW, imgH,
   getPixel,
   
   MImage, 
      
   mkMImage, mkImageTile, addSample, splatSample, addTile,
   
   imageWidth, imageHeight, sampleExtent, writePpm, 
   
   thaw, freeze,
   
   ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Debug.Trace
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as MV
import System.IO

import Graphics.Bling.Filter
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum
import Graphics.Bling.Types

-- | an image pixel, which consists of the sample weight, the sample RGB value
--   and the RGB value of the splatted samples
type SplatPixel = (Float, Float, Float)

emptySplat :: SplatPixel
emptySplat = (0, 0, 0)

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
   
   forM_ [0 .. (filterTableSize -1)] $ \y -> do
      let fy = (fromIntegral y + 0.5) * fh / fromIntegral filterTableSize
      
      forM_ [0 .. (filterTableSize -1)] $ \x -> do
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
   , _mSplatPixels   :: ! (MV.MVector (PrimState m) SplatPixel)
   }

-- | creates a new image where all pixels are initialized to black
mkMImage :: (PrimMonad m)
   => Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> m (MImage m)
mkMImage flt w h = do
   p <- MV.replicate (w * h * 4) 0
   s <- MV.replicate (w * h) emptySplat
   return $! MImage w h (0, 0) (mkTableFilter flt) p s
   
mkImageTile :: (PrimMonad m) => Image -> SampleWindow -> m (MImage m)
mkImageTile img wnd = do
   let
      w = xEnd wnd - px + floor (0.5 + fw)
      h = yEnd wnd - py + floor (0.5 + fh)
      px = max 0 $ xStart wnd -- + floor (0.5 - fw)
      py = max 0 $ yStart wnd -- + floor (0.5 - fh)
      (fw, fh) = tblFltSize $ imgFilter img
      
   p <- MV.replicate (w * h * 4) 0
   s <- MV.replicate (w * h) emptySplat
   return $! MImage w h (px, py) (imgFilter img) p s
   
-- | an immutable image
data Image = Img
   { imgW         :: {-# UNPACK #-} ! Int
   , imgH         :: {-# UNPACK #-} ! Int
   , imgFilter    :: {-# UNPACK #-} ! TableFilter
   , _imgP        :: ! (V.Vector Float)
   , _imgS        :: ! (V.Vector SplatPixel)
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
   s = V.replicate (w * h) emptySplat
   
-- | converts an image to a mutable image 
thaw :: (PrimMonad m) => Image -> m (MImage m)
thaw (Img w h f p s) = {-# SCC "thaw" #-} do
   p' <- GV.thaw p
   s' <- GV.thaw s
   return $! MImage w h (0, 0) f p' s'

-- | converts a mutable image to an image (and the offsets)
freeze :: (PrimMonad m) => MImage m -> m (Image, (Int, Int))
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
addTile (MImage w h (ox, oy) _ px ps) (Img tw th _ px' ps', (dx, dy)) = {-# SCC addTile #-} do
   forM_ [(x, y) | y <- [0 .. (th-1)], x <- [0 .. (tw-1)]] $ \(x, y) -> do
      let
         od    = w * (y - oy + dy) + (x - ox + dx)
         od'   = 4 * od
         os    = tw * y + x
         os'   = 4 * os
         
      unless ((y - oy + dy) >= h || (x - ox + dx) >= w) $ do
         -- the splats
         MV.unsafeRead ps od >>= \old -> MV.unsafeWrite ps od $ spAdd old (V.unsafeIndex ps' os)
         
         -- the pixels
         forM_ [0..3] $ \o -> do
            old <- MV.unsafeRead px (od' + o)
            MV.unsafeWrite px (od' + o) (old + V.unsafeIndex px' (os' + o))
            
spAdd :: SplatPixel -> SplatPixel -> SplatPixel
spAdd (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

splatSample :: PrimMonad m => MImage m -> ImageSample -> m ()
{-# INLINE splatSample #-}
splatSample (MImage w h (iox, ioy) _ _ p) (sx, sy, WS sw ss)
   | floor sx >= w || floor sy >= h || sx < 0 || sy < 0 = return ()
   | sNaN ss = trace ("not splatting NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("not splatting infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | isNaN sw = trace ("not splatting NaN weight sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | isInfinite sw = trace ("not splatting infinite weight sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "splatSample" #-} do
      (ox, oy, oz) <- MV.unsafeRead p o
      MV.unsafeWrite p o (ox + dx, oy + dy, oz + dz)
      where
         o = ((floor sx - iox) + (floor sy - ioy) * w)
         (dx, dy, dz) = (\(x, y, z) -> (x * sw, y * sw, z * sw)) $ spectrumToXYZ ss


-- | adds a sample to the specified image
addSample :: PrimMonad m => MImage m -> Float -> Float -> Spectrum -> m ()
{-# INLINE addSample #-}
addSample (MImage !w !h (!ox, !oy) ftbl !p _) sx sy ss
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = do
   
      let
         (smx, smy, smz) = spectrumToXYZ ss
         (fw, fh) = tblFltSize ftbl
         ifw = 1 / fw
         ifh = 1 / fw
         
         dx = sx - 0.5
         dy = sy - 0.5
         
         x0 = max ox (ceiling $ dx - fw)
         x1 = min (ox + w-1) (floor $ dx + fw)
         y0 = max oy (ceiling $ dy - fh)
         y1 = min (oy + h-1) (floor $ dy + fw)
      
      unless ((x1 - x0) < 0 || (y1 - y0) < 0) $ do
         let 
            ifx = V.generate (x1 - x0 + 1) $
               \x -> let x' = x + x0 in let fx = abs $ (fromIntegral x' - dx) * ifw * fromIntegral filterTableSize
                     in min (floor fx) (filterTableSize - 1)
            
            ify = V.generate (y1 - y0 + 1) $
               \y -> let y' = y + y0 in let fy = abs $ (fromIntegral y' - dy) * ifh * fromIntegral filterTableSize
                     in min (floor fy) (filterTableSize - 1)
            
         forM_ [(x, y) | y <- [y0..y1], x <- [x0..x1]] $ \(x, y) -> do
            let 
               imgo = 4 * ((x - ox) + (y - oy) * w)
               flto = V.unsafeIndex ify (y - y0) * filterTableSize + V.unsafeIndex ifx (x-x0)
               fltw = V.unsafeIndex (tblVals ftbl) flto
               
            ow <- MV.unsafeRead p imgo
            orx <- MV.unsafeRead p $ (imgo + 1)
            ory <- MV.unsafeRead p $ (imgo + 2)
            orz <- MV.unsafeRead p $ (imgo + 3)
            
            MV.unsafeWrite p imgo $ ow + fltw
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
   | w == 0 = xyzToRgb (sw * sr, sw * sg, sw * sb)
   | otherwise = xyzToRgb (sw * sr + r / w, sw * sg + g / w, sw * sb + b / w)
   where
      o' = 4 * o
      (w, r, g, b) = (p V.! o', p V.! (o' + 1), p V.! (o' + 2), p V.! (o' + 3))
      (sr, sg, sb) = s V.! o
      
-- | writes an image in ppm format
writePpm
   :: Image
   -> Float -- ^ splat weight
   -> Handle
   -> IO ()
writePpm img@(Img w h _ _ _) splatW handle =
   let
       header = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"
       pixel p = return $ ppmPixel $ getPixel img splatW p
   in do
      hPutStr handle header
      mapM_ (\p -> pixel p >>= hPutStr handle) [0..(w*h-1)]
      
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

-- | converts a @WeightedSpectrum@ into what's expected to be found in a ppm file
ppmPixel :: (Float, Float, Float) -> String
ppmPixel ws = (toString . gamma 2.2) ws
   where
      toString (r, g, b) = show (clamp r) ++ " " ++ show (clamp g) ++ " " ++ show (clamp b) ++ " "

