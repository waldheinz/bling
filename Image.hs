
module Image(
   Image, ImageSample(..),
   mkImage,
   imageWidth, imageHeight, 
   imageToPpm, addSample) where

import Debug.Trace

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import Color

-- | places a @WeightedSpectrum@ in an @Image@
data ImageSample = ImageSample {
   samplePosX :: ! Float,
   samplePosY :: ! Float,
   sampleSpectrum :: ! WeightedSpectrum
   } deriving Show
   
-- | an image has a width, a height and some pixels 
data Image s = Image {
   imageWidth :: Int,
   imageHeight :: Int,
   _imagePixels :: (STUArray s Int Float)
   }
   
mkImage :: Int -> Int -> ST s (Image s)
mkImage w h = do
   pixels <- newArray (0, (w * h * 4)) 0.0 :: ST s (STUArray s Int Float)
   return $ Image w h pixels
   
addPixel :: Image s -> Int -> WeightedSpectrum -> ST s (Image s)
addPixel (Image w h p) o (sw, s) = do
   osw <- readArray p o'
   writeArray p o' (osw + sw)
   return $ Image w h p
   where
         (sx, sy, sz) = toXyz s
         o' = o * 4


-- | adds an sample to the specified image and returns the updated image
addSample :: Image s -> ImageSample -> Image s
addSample img@(Image w h _) (ImageSample sx sy (sw, ss))
   | isx > maxX || isy > maxY = img
   | sNaN ss = trace ("skipping NaN sample at (" ++ (show sx) ++ ", " ++ (show sy) ++ ")") img
   | otherwise = img
   where
      img' = img -- seq img seq newPixel putPixel img offset newPixel
      isx = floor sx
      isy = floor sy
      maxX = w - 1
      maxY = h - 1
      offset = isy * w + isx
  --    (oldW, oldS) = getPixel img offset
  --    newPixel = (oldW + sw, oldS + ss)


-- | extracts the pixel at the specified offset from an Image
getPixel :: Image s -> Int -> ST s WeightedSpectrum
getPixel (Image _ _ p) o = do
   w <- readArray p o'
   x <- readArray p (o' + 1)
   y <- readArray p (o' + 2)
   z <- readArray p (o' + 3)
   return $ (w, fromXyz (x, y, z)) where
      o' = o * 4
   
{-
-- | puts an pixel to the specified offset in an Image
putPixel :: Image -> Int -> WeightedSpectrum -> Image
putPixel (Image w h p) o (sw, s) = seq p' Image w h p' where
   (sx, sy, sz) = toXyz s
   p' = p // [ (o', sw), (o' + 1, sx), (o' + 2, sy), (o' + 3, sz) ]
   o' = o * 4
  
-}

-- | converts an image to ppm format
imageToPpm :: Image s -> ST s String
imageToPpm i@(Image w h _) = do
   pData <- spixels 0
   return $ "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ pData
   where
      spixels pos
         | pos == (w*h) = return $ []
         | otherwise = do
            p <- getPixel i pos
            rest <- spixels (pos+1)
            return $ (ppmPixel $ p) ++ rest

-- | applies gamma correction to an RGB triple
gamma :: Float -> (Float, Float, Float) -> (Float, Float, Float)
gamma x (r, g, b) = (r ** x', g ** x', b ** x') where
   x' = 1 / x

-- | converts a Float in [0..1] to an Int in [0..255], clamping values outside [0..1]
clamp :: Float -> Int
clamp v = round ( (min 1 (max 0 v)) * 255 )

-- | converts a @WeightedSpectrum@ into what's expected to be found in a ppm file
ppmPixel :: WeightedSpectrum -> String
ppmPixel ws = (toString . (gamma 2.2) .toRgb . mulWeight) ws
   where
      toString (r, g, b) = show (clamp r) ++ " " ++ show (clamp g) ++ " " ++ show (clamp b) ++ " "

-- | converts a weighted spectrum to a plain spectrum by dividing out the weight
mulWeight :: WeightedSpectrum -> Spectrum
mulWeight (0, _) = black
mulWeight (w, s) = sScale s (1.0 / w)
