
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Graphics.Bling.Image (
 --  module Graphics.Bling.Filter,
   
   Image, ImageSample, mkImage, rgbPixels, imageWindow', imgW, imgH, imgFilter,
   
   MImage, imageFilter,
   
   mkMImage, mkImageTile, addSample, splatSample, addContrib, addTile,
   
   imageWidth, imageHeight, sampleExtent, writePpm, 
   
   thaw, freeze,
   
   -- * Pixel Filters
   Filter, mkBoxFilter, mkTriangleFilter, mkMitchellFilter, mkSincFilter,
   mkGaussFilter,
   
   -- * Reading and Writing RGBE
   writeRgbe
   
   ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Debug.Trace
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Lazy as BS
import System.IO

import Graphics.Bling.Sampling
import Graphics.Bling.Types
import Graphics.Bling.Spectrum

-- | an image pixel, which consists of the sample weight, the sample RGB value
--   and the RGB value of the splatted samples
type SplatPixel = (Float, Float, Float)

emptySplat :: SplatPixel
emptySplat = (0, 0, 0)

-- | a mutable image
data MImage m = MImage
   { imageWidth      :: {-# UNPACK #-} ! Int
   , imageHeight     :: {-# UNPACK #-} ! Int
   , _imageOffset    :: {-# UNPACK #-} ! (Int, Int)
   , imageFilter     :: ! Filter
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
   return $! MImage w h (0, 0) flt p s

mkImageTile :: (PrimMonad m) => Filter -> SampleWindow -> m (MImage m)
mkImageTile f wnd = do
   let
      w = xEnd wnd - px + floor (0.5 + fw)
      h = yEnd wnd - py + floor (0.5 + fh)
      px = max 0 $ xStart wnd -- + floor (0.5 - fw)
      py = max 0 $ yStart wnd -- + floor (0.5 - fh)
      (fw, fh) = filterSize f

   p <- MV.replicate (w * h * 4) 0
   s <- MV.replicate (w * h) emptySplat
   return $! MImage w h (px, py) f p s
   
-- | an immutable image
data Image = Img
   { imgW      :: {-# UNPACK #-} ! Int
   , imgH      :: {-# UNPACK #-} ! Int
   , imgFilter :: ! Filter
   , _imgP     :: ! (V.Vector Float)
   , _imgS     :: ! (V.Vector SplatPixel)
   }

instance NFData Image where
   rnf (Img w h f p s) = w `seq` h `seq` f `seq` p `seq` s `seq ` ()

instance Show Image where
   show (Img w h f _ _) = "Image {width=" ++ (show w) ++ ", height=" ++ (show h) ++ ", filter=" ++ (show f) ++ "}"

-- | creates a new image where all pixels are initialized to black
mkImage
   :: Filter      -- ^ the pixel filter function to use when adding samples
   -> PixelSize   -- ^ the image width
   -> Image
mkImage flt (w, h) = Img w h flt p s where
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
   
sampleExtent :: PrimMonad m => MImage m -> SampleWindow
sampleExtent (MImage w h (ox, oy) f _ _) = SampleWindow x0 x1 y0 y1 where
   x0 = ox + floor (0.5 - fw)
   x1 = ox + floor (0.5 + (fromIntegral w) + fw)
   y0 = oy + floor (0.5 - fh)
   y1 = oy + floor (0.5 + (fromIntegral h) + fh)
   (fw, fh) = filterSize f
   
imageWindow' :: Image -> SampleWindow
imageWindow' (Img w h f _ _) = SampleWindow x0 x1 y0 y1 where
   x0 = floor (0.5 - fw)
   x1 = floor (0.5 + (fromIntegral w) + fw)
   y0 = floor (0.5 - fh)
   y1 = floor (0.5 + (fromIntegral h) + fh)
   (fw, fh) = filterSize f
   
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

addPixel :: PrimMonad m => MImage m -> (Int, Int, WeightedSpectrum) -> m ()
{-# INLINE addPixel #-}
addPixel !(MImage !w !h (!ox, !oy) _ !p _) (!x, !y, WS !sw !s)
   | (x - ox) < 0 || (y - oy) < 0 = return ()
   | x >= (w + ox) || y >= (h + oy) = return ()
   | otherwise = do
      let
         o' = 4 * ((x - ox) + (y - oy) * w)
         (r', g', b') = spectrumToXYZ s
      
      ow <- MV.unsafeRead p o'
      r <- MV.unsafeRead p $ (o' + 1)
      g <- MV.unsafeRead p $ (o' + 2)
      b <- MV.unsafeRead p $ (o' + 3)
      
      MV.unsafeWrite p o' $ ow + sw
      MV.unsafeWrite p (o' + 1) $ (r + r')
      MV.unsafeWrite p (o' + 2) $ (g + g')
      MV.unsafeWrite p (o' + 3) $ (b + b')
      
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
addSample :: PrimMonad m => MImage m -> ImageSample -> m ()
addSample !img smp@(!sx, !sy, !WS !sw !ss)
   | sw == 0 = return ()
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "addSample" #-} filterSample (imageFilter img) smp img

addContrib :: PrimMonad m => MImage m -> Contribution -> m ()
{-# INLINE addContrib #-}
addContrib !img (!splat, !is)
   | splat = {-# SCC "addContrib.splat" #-} splatSample img is
   | otherwise = {-# SCC "addContrib.sample" #-} addSample img is

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

--------------------------------------------------------------------------------
-- RGBE
--------------------------------------------------------------------------------

frexp :: Float -> (Float, Int)
frexp x
   | isNaN x = error "NaN given to frexp"
   | isInfinite x = error "infinity given to frexp"
   | otherwise  = frexp' (x, 0) where
      frexp' (s, e)
         | s >= 1.0 = frexp' (s / 2, e + 1)
         | s < 0.5 = frexp' (s * 2, e - 1)
         | otherwise = (s, e)

writeRgbe :: Image -> Float -> Handle -> IO ()
writeRgbe img@(Img w h _ _ _) spw hnd =
   let header = "#?RGBE\nFORMAT=32-bit_rgbe\n\n-Y " ++ show h ++ " +X " ++ show w ++ "\n"
       pixel :: Int -> IO BS.ByteString
       pixel p = return $ toRgbe $ getPixel img spw p
   in do
      hPutStr hnd header
      mapM_ (\p -> pixel p >>= BS.hPutStr hnd) [0..(w*h-1)]

toRgbe :: (Float, Float, Float) -> BS.ByteString
toRgbe (r, g, b)
   | v < 0.00001 = BS.pack [0,0,0,0]
   | otherwise = BS.pack $ map truncate [r * v'', g * v'', b * v'', fromIntegral $ e + 128]
   where
         v = max r $ max g b
         (v', e) = frexp v
         v'' = v' * 256 / v
         
--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

data Filter
   = Box
   | Gauss {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
           {-# UNPACK #-} !Float {-# UNPACK #-} !Float -- w, h, expX, expY, alpha
   | Sinc {-# UNPACK #-} !Float {-# UNPACK #-} !Float
          {-# UNPACK #-} !Float {-# UNPACK #-} !Float
          {-# UNPACK #-} !Float -- xw, xy, invx, invy, tau
   | Mitchell {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
   | Triangle {-# UNPACK #-} !Float {-# UNPACK #-} !Float
   deriving (Show)
   
-- | creates a box filter
mkBoxFilter :: Filter
mkBoxFilter = Box 

mkGaussFilter :: Float -> Float -> Float -> Filter
mkGaussFilter xw yw alpha = Gauss xw yw (exp $ -alpha * xw * xw) (exp $ -alpha * yw * yw) alpha

-- | creates a Lanczos - Sinc filter
mkSincFilter
   :: Float -- ^ the filter extent's width
   -> Float -- ^ the filter extent's height
   -> Float -- ^ the tau parameter for the filter, controls how many
            --   cycles the sinc passes through before camped to zero
   -> Filter
mkSincFilter wx wy tau = Sinc wx wy (1 / wx) (1 / wy) tau

-- | creates a triangle filter
mkTriangleFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Filter

mkTriangleFilter = Triangle

-- | creates a mitchell filter
mkMitchellFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Float -- ^ the Mitchell "B" parameter
   -> Float -- ^ the Mitchell "C" parameter
   -> Filter
   
mkMitchellFilter = Mitchell   

filterSize :: Filter -> (Float, Float)
{-# INLINE filterSize #-}
filterSize (Box)              = (0.5, 0.5)
filterSize (Gauss w h _ _ _)  = (w, h)
filterSize (Sinc w h _ _ _)   = (w, h)
filterSize (Mitchell w h _ _) = (w, h)
filterSize (Triangle w h)     = (w, h)

filterSample :: (PrimMonad m) => Filter -> ImageSample -> MImage m -> m ()
{-# INLINE filterSample #-}
filterSample Box (x, y, ws) img = addPixel img (floor x, floor y, ws)
filterSample f (ix, iy, WS sw s) img = do
   let
      (# dx, dy #) = (# ix - 0.5, iy - 0.5 #)
      (# x0, x1 #) = (# ceiling (dx - fw), floor (dx + fw) #)
      (# y0, y1 #) = (# ceiling (dy - fh), floor (dy + fh) #)
      w !x !y = evalFilter f (fromIntegral x - ix) (fromIntegral y - iy)
      (fw, fh) = filterSize f

   forM_ [(x, y) | y <- [y0..y1], x <- [x0..x1]] $ \(x, y) ->
      let wt = w x y in addPixel img (x, y, WS (sw * wt) $ sScale s wt)

evalFilter :: Filter -> Float -> Float -> Float
{-# INLINE evalFilter #-}
evalFilter (Gauss _ _ ex ey a) x y = gaussian x ex * gaussian y ey where
   gaussian d expv = max 0 $ exp (-a * d * d) - expv
evalFilter (Mitchell w h b c) px py = m1d (px * iw) * m1d (py * ih) where
   (iw, ih) = (1 / w, 1 / h)
   m1d x' = y where
      x = abs (2 * x')
      y = if x > 1
             then (((-b) - 6*c) * x*x*x + (6*b + 30*c) * x*x +
                    ((-12)*b - 48*c) * x + (8*b + 24*c)) * (1/6)
             else ((12 - 9*b - 6*c) * x*x*x +
                   ((-18) + 12*b + 6*c) * x*x +
                    (6 - 2*b)) * (1/6)

evalFilter (Sinc _ _ ix iy tau) px py = sinc1D (px * ix) * sinc1D (py * iy) where
   sinc1D x
      | abs x > 1 = 0
      | abs x < 1e-5 = 1
      | otherwise = sinc * lanczos where
         x' = abs x * pi
         lanczos = sin (x' * tau) / (x' * tau)
         sinc = sin x' / x'
         
evalFilter (Triangle w h) x y = f (x, y) where
   f (px, py) = max 0 (w - abs px) * max 0 (h - abs py)

evalFilter f _ _ =
   error ("evalFilter for " ++ show f ++ " called")


