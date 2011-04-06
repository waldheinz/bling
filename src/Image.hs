
module Image(
   Image, ImageSample(..), 
   mkImage,
   imageWidth, imageHeight, 
   writePpm, writeRgbe, addSample) where

import Control.Monad
import Control.Monad.ST
import Debug.Trace
import Data.Vector.Unboxed.Mutable as V
import qualified Data.ByteString as BS
import System.IO

import Filter
import Spectrum

-- | an image has a width, a height and some pixels 
data Image s = Image {
   imageWidth :: Int,
   imageHeight :: Int,
   imageFilter :: Filter,
   _imagePixels :: MVector s (Float, Float, Float, Float)
   }

mkImage :: Filter -> Int -> Int -> ST s (Image s)
mkImage flt w h = do
   pixels <- V.replicate (w * h) (0, 0, 0, 0)
   return $ Image w h flt pixels

addPixel :: Image s -> (Int, Int, WeightedSpectrum) -> ST s ()
{-# INLINE addPixel #-}
addPixel (Image w h _ p) (x, y, (sw, s))
   | x < 0 || y < 0 = return ()
   | x >= w || y >= h = return ()
   | otherwise = do
      px <- unsafeRead p o
      unsafeWrite p o (add dpx px)
   where
         dpx = (sw, r, g, b)
         (r, g, b) = toRGB s
         add (w1, r1, g1, b1) (w2, r2, g2, b2) =
              (w1+w2, r1+r2, g1+g2, b1+b2)
         o = (x + y*w)
         
-- | adds a sample to the specified image
addSample :: Image s -> ImageSample -> ST s ()
addSample img smp@(ImageSample sx sy (_, ss))
   | sNaN ss = trace ("skipping NaN sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | sInfinite ss = trace ("skipping infinite sample at ("
      ++ show sx ++ ", " ++ show sy ++ ")") (return () )
   | otherwise = mapM_ (addPixel img) pixels
   where
         pixels = filterSample (imageFilter img) smp

-- | extracts the pixel at the specified offset from an Image
getPixel :: Image s -> Int -> ST s WeightedSpectrum
getPixel (Image _ _ _ p) o = do
   (w, r, g, b) <- unsafeRead p o
   return (w, fromRGB (r, g, b))
   
writeRgbe :: Image RealWorld -> Handle -> IO ()
writeRgbe img@(Image w h _ _) hnd =
   let header = "#?RGBE\nFORMAT=32-bit_rgbe\n\n-Y " ++ show h ++ " +X " ++ show w ++ "\n"
       pixel p = stToIO $ liftM (toRgbe . toRGB . mulWeight) $ getPixel img p
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

-- | converts a @WeightedSpectrum@ into what's expected to be found in a ppm file
ppmPixel :: WeightedSpectrum -> String
ppmPixel ws = (toString . gamma 2.2 .toRGB . mulWeight) ws
   where
      toString (r, g, b) = show (clamp r) ++ " " ++ show (clamp g) ++ " " ++ show (clamp b) ++ " "

-- | converts a weighted spectrum to a plain spectrum by dividing out the weight
mulWeight :: WeightedSpectrum -> Spectrum
mulWeight (0, _) = black
mulWeight (w, s) = sScale s (1.0 / w)
