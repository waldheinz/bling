{-# LANGUAGE RankNTypes #-}

module Graphics.Bling.Image (
   Image, ImageSample(..),
   
   mkImage, addSample, splatSample,
   
   imageWidth, imageHeight, imageWindow, writePpm, writeRgbe, rgbPixels
   ) where

import Control.Monad
import Control.Monad.ST
import Debug.Trace
import Data.Vector.Unboxed.Mutable as V
import qualified Data.ByteString as BS
import System.IO

import Graphics.Bling.Filter
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum

-- | an image pixel, which consists of the sample weight, the sample XYZ value
--   and the XYZ value of the splatted samples
type Pixel = (Float, (Float, Float, Float), (Float, Float, Float))

-- | an image has a width, a height and some pixels 
data Image s = Image {
   imageWidth :: Int,
   imageHeight :: Int,
   imageFilter :: Filter,
   _imagePixels :: MVector s Pixel
   }

-- | creates a new image where all pixels are initialized to black
mkImage
   :: Filter -- ^ the pixel filter function to use when adding samples
   -> Int -- ^ the image width
   -> Int -- ^ the image height
   -> ST s (Image s)
mkImage flt w h = do
   pixels <- V.replicate (w * h) (0, (0, 0, 0), (0, 0, 0))
   return $ Image w h flt pixels

imageWindow :: Image s -> SampleWindow
imageWindow (Image w h f _) = SampleWindow x0 x1 y0 y1 where
   x0 = floor (0.5 - filterWidth f)
   x1 = floor (0.5 + (fromIntegral w) + filterWidth f)
   y0 = floor (0.5 - filterHeight f)
   y1 = floor (0.5 + (fromIntegral h) + filterHeight f)

addPixel :: Image s -> (Int, Int, WeightedSpectrum) -> ST s ()
{-# INLINE addPixel #-}
addPixel (Image w h _ p) (x, y, (sw, s))
   | x < 0 || y < 0 = return ()
   | x >= w || y >= h = return ()
   | otherwise = do
      px <- unsafeRead p o
      unsafeWrite p o (add px dpx)
   where
         dpx = (sw, r, g, b)
         (r, g, b) = toRGB s
         add (w1, (r1, g1, b1), splat) (w2, r2, g2, b2) =
              (w1+w2, (r1+r2, g1+g2, b1+b2), splat)
         o = (x + y*w)
         
splatSample :: Image s -> ImageSample -> ST s ()
splatSample (Image w h _ p) (ImageSample sx sy (sw, ss))
   | floor sx > w || floor sy > h || sx < 0 || sy < 0 = trace ("ignore splat at (" ++ 
      show sx ++ ", " ++ show sy ++ ")") $ return ()
   | sNaN ss = trace ("not splatting NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("not splatting infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = do
      (ow, oxyz, (ox, oy, oz)) <- unsafeRead p o
      unsafeWrite p o (ow, oxyz, (ox + dx, oy + dy, oz + dz))
      where
         o = (floor sx + floor sy * w)
         (dx, dy, dz) = (\(x, y, z) -> (x * sw, y * sw, z * sw)) $ toRGB ss
         
-- | adds a sample to the specified image
addSample :: forall s. Image s -> ImageSample -> ST s ()
addSample img smp@(ImageSample sx sy (_, ss))
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = mapM_ (addPixel img) pixels
   where
         pixels = filterSample (imageFilter img) smp

-- | extracts the pixel at the specified offset from an Image
getPixel :: Image s -> Int -> ST s (Float, Float, Float)
getPixel (Image _ _ _ p) o = do
   (w, (r, g, b), (sr, sg, sb)) <- unsafeRead p o
   return $ if w == 0 
               then (sr, sg, sb)
               else (sr + r / w, sg + g / w, sb + b / w)
   
writeRgbe :: Image RealWorld -> Handle -> IO ()
writeRgbe img@(Image w h _ _) hnd =
   let header = "#?RGBE\nFORMAT=32-bit_rgbe\n\n-Y " ++ show h ++ " +X " ++ show w ++ "\n"
       pixel p = stToIO $ liftM toRgbe $ getPixel img p
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
         v'' = v' * 256.0 / v

frexp :: Float -> (Float, Int)
frexp x
   | isNaN x = error "NaN given to frexp"
   | isInfinite x = error "infinity given to frexp"
   | otherwise  = frexp' (x, 0) where
      frexp' (s, e)
         | s >= 1.0 = frexp' (s / 2, e + 1)
         | s < 0.5 = frexp' (s * 2, e - 1)
         | otherwise = (s, e)

-- | writes an image in ppm format
writePpm :: Image RealWorld -> Handle -> IO ()
writePpm img@(Image w h _ _) handle =
   let
       header = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"
       pixel p = stToIO $ liftM ppmPixel $ getPixel img p
   in do
      hPutStr handle header
      mapM_ (\p -> pixel p >>= hPutStr handle) [0..(w*h-1)]
      
-- | applies gamma correction to an RGB triple
gamma :: Float -> (Float, Float, Float) -> (Float, Float, Float)
gamma x (r, g, b) = (r ** x', g ** x', b ** x') where
   x' = 1 / x

-- | converts a Float in [0..1] to an Int in [0..255], clamping values outside [0..1]
clamp :: Float -> Int
clamp v = round ( min 1 (max 0 v) * 255 )

rgbPixels :: Image s -> SampleWindow -> ST s [((Int, Int), (Int, Int, Int))]
rgbPixels img w = do
   ps <- mapM (getPixel img) os
   let rgbs = map (gamma 2.2) ps
   let clamped = map (\(r,g,b) -> (clamp r, clamp g, clamp b)) rgbs
   return $ Prelude.zip xs $ clamped
   where
         xs = coverWindow w
         os = map (\(x,y) -> (y * (imageWidth img)) + x) xs
      
-- | converts a @WeightedSpectrum@ into what's expected to be found in a ppm file
ppmPixel :: (Float, Float, Float) -> String
ppmPixel ws = (toString . gamma 2.2) ws
   where
      toString (r, g, b) = show (clamp r) ++ " " ++ show (clamp g) ++ " " ++ show (clamp b) ++ " "
