
module Image(Image, ImageSample(..), imageWidth, imageHeight, imageToPpm, makeImage, addSample) where

import Debug.Trace
import Data.Array.Diff
import Data.Array.IO

import Color
import Math

data ImageSample = ImageSample {
   samplePosX :: ! Float,
   samplePosY :: ! Float,
   sampleSpectrum :: ! WeightedSpectrum
   } deriving Show

data Image = Image {
   imageWidth :: Int,
   imageHeight :: Int,
   imagePixels :: (DiffUArray Int Float)
   }
   
getPixel :: Image -> Int -> WeightedSpectrum
getPixel (Image _ _ p) o = (p ! o', (p ! (o' + 1), p ! (o' + 2), p ! (o' + 3))) where
   o' = o * 4
   
putPixel :: Image -> Int -> WeightedSpectrum -> Image
putPixel (Image w h p) o (sw, (sr, sg, sb)) = seq p' Image w h p' where
   p' = p // [ (o', sw), (o' + 1, sr), (o' + 2, sg), (o' + 3, sb) ]
   o' = o * 4
   
imageToPpm :: Image -> String
imageToPpm i@(Image w h p) = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ spixels 0
   where
      spixels pos
         | pos == (w*h) = []
         | otherwise = (ppmPixel $ getPixel i pos) ++ spixels (pos + 1)

ppmPixel :: WeightedSpectrum -> String
ppmPixel ws = toString $ mulWeight ws
   where
      toString (r, g, b) = show (clamp r) ++ " " ++ show (clamp g) ++ " " ++ show (clamp b) ++ " "

mulWeight :: WeightedSpectrum -> Spectrum
mulWeight (0, _) = black
mulWeight (w, s) = scalMul s (1.0 / w)

addSample :: Image -> ImageSample -> Image
addSample img@(Image w h pixels) (ImageSample sx sy (sw, ss))
   | isx > maxX || isy > maxY = img
   | otherwise = putPixel img offset newPixel
   where
      isx = floor sx
      isy = floor sy
      maxX = w - 1
      maxY = h - 1
      offset = isy * w + isx
      (oldW, oldS) = getPixel img offset
      newPixel = (oldW + sw, add oldS ss)

makeImage :: Int -> Int -> Image
makeImage w h = Image w h pixels where
    pixels = listArray (0, pxCount - 1) (repeat 0.0)
    pxCount = w * h * 4 :: Int

clamp :: Float -> Int
clamp v = round ( (min 1 (max 0 v)) * 255 )
    