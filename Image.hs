
module Image(
   Image, ImageSample(..), 
   imageWidth, imageHeight, 
   imageToPpm, makeImage, addSample) where

import Data.Array.Diff

import Color

-- | places a @WeightedSpectrum@ in an @Image@
data ImageSample = ImageSample {
   samplePosX :: ! Float,
   samplePosY :: ! Float,
   sampleSpectrum :: ! WeightedSpectrum
   } deriving Show

-- | an image has a width, a height and some pixels
data Image = Image {
   imageWidth :: Int,
   imageHeight :: Int,
   _imagePixels :: (DiffUArray Int Float)
   }
   
-- | extracts the pixel at the specified offset from an Image
getPixel :: Image -> Int -> WeightedSpectrum
getPixel (Image _ _ p) o = (p ! o', s) where
   s = fromXyz (p ! (o' + 1), p ! (o' + 2), p ! (o' + 3))
   o' = o * 4
   
-- | puts an pixel to the specified offset in an Image
putPixel :: Image -> Int -> WeightedSpectrum -> Image
putPixel (Image w h p) o (sw, s) = seq p' Image w h p' where
   (sx, sy, sz) = toXyz s
   p' = p // [ (o', sw), (o' + 1, sx), (o' + 2, sy), (o' + 3, sz) ]
   o' = o * 4
   
-- | converts an image to ppm format
imageToPpm :: Image -> String
imageToPpm i@(Image w h _) = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ spixels 0
   where
      spixels pos
         | pos == (w*h) = []
         | otherwise = (ppmPixel $ getPixel i pos) ++ spixels (pos + 1)

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

-- | adds an sample to the specified image and returns the updated image
addSample :: Image -> ImageSample -> Image
addSample img@(Image w h _) (ImageSample sx sy (sw, ss))
   | isx > maxX || isy > maxY = img
   | otherwise = seq img' img'
   where
      img' = seq img seq newPixel putPixel img offset newPixel
      isx = floor sx
      isy = floor sy
      maxX = w - 1
      maxY = h - 1
      offset = isy * w + isx
      (oldW, oldS) = getPixel img offset
      newPixel = (oldW + sw, oldS + ss)

-- | creates a new all-black image of the specified width and height
makeImage :: Int -> Int -> Image
makeImage w h = Image w h pixels where
    pixels = listArray (0, pxCount - 1) (repeat 0.0)
    pxCount = w * h * 4 :: Int
