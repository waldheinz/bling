
module Graphics.Bling.Image (
   module Graphics.Bling.Filter,
   
   Image, ImageSample(..), mkImage, rgbPixels, imageWindow', imgW, imgH,
   
   MImage,
   
   mkMImage, addSample, splatSample, addContrib,
   
   imageWidth, imageHeight, imageWindow, writePpm, 
   
   thaw, freeze,

   -- * Reading and Writing RGBE

   writeRgbe
   
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Debug.Trace
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as V 
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Lazy as BS
import System.IO

import Graphics.Bling.Filter
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum

-- | an image pixel, which consists of the sample weight, the sample RGB value
--   and the RGB value of the splatted samples
type Pixel = (Float, (Float, Float, Float), (Float, Float, Float))

-- | a mutable image in the ST monad
data MImage s = MImage
   { imageWidth   :: {-# UNPACK #-} ! Int
   , imageHeight  :: {-# UNPACK #-} ! Int
   , imageOffset  :: {-# UNPACK #-} ! (Int, Int)
   , imageFilter  :: ! Filter
   , _imagePixels :: MV.MVector (PrimState (ST s)) Pixel
   }

-- | creates a new image where all pixels are initialized to black
mkMImage
   :: Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> ST s (MImage s)
mkMImage flt w h = MImage w h (0, 0) flt <$> MV.replicate (w * h) (0, (0, 0, 0), (0, 0, 0))

-- | an immutable image
data Image = Img
   { imgW :: {-# UNPACK #-} ! Int
   , imgH :: {-# UNPACK #-} ! Int
   , _imgF :: ! Filter
   , _imgP :: V.Vector Pixel
   }

-- | creates a new image where all pixels are initialized to black
mkImage
   :: Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> Image
mkImage flt w h = Img w h flt $ V.replicate (w * h) (0, (0, 0, 0), (0, 0, 0))

-- | converts an image to a mutable image 
thaw :: Image -> ST s (MImage s)
thaw (Img w h f p) = {-# SCC "thaw" #-} do
   p' <- GV.thaw p
   return $ MImage w h (0, 0) f p'
 
-- | converts a mutable image to an image (and the offsets)
freeze :: MImage s -> ST s (Image, (Int, Int))
freeze (MImage w h o f p) = {-# SCC "freeze" #-} (\p' -> (Img w h f p', o)) <$> GV.freeze p

imageWindow :: MImage s -> SampleWindow
imageWindow (MImage w h (ox, oy) f _) = SampleWindow x0 x1 y0 y1 where
   x0 = ox + floor (0.5 - filterWidth f)
   x1 = ox + floor (0.5 + (fromIntegral w) + filterWidth f)
   y0 = oy + floor (0.5 - filterHeight f)
   y1 = oy + floor (0.5 + (fromIntegral h) + filterHeight f)

imageWindow' :: Image -> SampleWindow
imageWindow' (Img w h f _) = SampleWindow x0 x1 y0 y1 where
   x0 = floor (0.5 - filterWidth f)
   x1 = floor (0.5 + (fromIntegral w) + filterWidth f)
   y0 = floor (0.5 - filterHeight f)
   y1 = floor (0.5 + (fromIntegral h) + filterHeight f)

addPixel :: MImage s -> (Int, Int, WeightedSpectrum) -> ST s ()
{-# INLINE addPixel #-}
addPixel (MImage w h (ox, oy) _ p) (x, y, (sw, s))
   | x < 0 || y < 0 = return ()
   | x >= w || y >= h = return ()
   | otherwise = do
      px <- MV.unsafeRead p o
      MV.unsafeWrite p o (add px dpx)
   where
         dpx = (sw, r, g, b)
         (r, g, b) = toRGB s
         add (w1, (r1, g1, b1), splat) (w2, r2, g2, b2) =
              (w1+w2, (r1+r2, g1+g2, b1+b2), splat)
         o = ((x - ox) + (y - oy) * w)
         
splatSample :: MImage s -> ImageSample -> ST s ()
splatSample (MImage w h (ox, oy) _ p) (ImageSample sx sy (sw, ss))
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
      (ow, oxyz, (ox, oy, oz)) <- MV.unsafeRead p o
      MV.unsafeWrite p o (ow, oxyz, (ox + dx, oy + dy, oz + dz))
      where
         o = ((floor sx - ox) + (floor sy - oy) * w)
         (dx, dy, dz) = (\(x, y, z) -> (x * sw, y * sw, z * sw)) $ toRGB ss

-- | adds a sample to the specified image
addSample :: MImage s -> ImageSample -> ST s ()
addSample img smp@(ImageSample sx sy (sw, ss))
   | sw == 0 = return ()
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = {-# SCC "addSample" #-} forM_ pixels (addPixel img)
   where
         pixels = filterSample (imageFilter img) smp

addContrib :: MImage s -> Contribution -> ST s ()
addContrib img (splat, is)
   | splat = splatSample img is
   | otherwise = addSample img is

-- | extracts the pixel at the specified offset from an Image
getPixel
   :: Image
   -> Float -- ^ splat weight
   -> Int
   -> (Float, Float, Float)
getPixel (Img _ _ _ p) sw o
   | w == 0 = (sw * sr, sw * sg, sw * sb)
   | otherwise = (sw * sr + r / w, sw * sg + g / w, sw * sb + b / w)
   where
      (w, (r, g, b), (sr, sg, sb)) = V.unsafeIndex p o
      
-- | writes an image in ppm format
writePpm
   :: Image
   -> Float -- ^ splat weight
   -> Handle
   -> IO ()
writePpm img@(Img w h _ _) splatW handle =
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
rgbPixels img@(Img w h _ _) spw wnd = Prelude.zip xs clamped where
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
writeRgbe img@(Img w h _ _) spw hnd =
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
