
module Image(Image, ImageSample(..), imageWidth, imageHeight, imageToPpm, makeImage, addSample) where

import Debug.Trace
import Data.Array.Diff

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
   imagePixels :: (Array Int WeightedSpectrum)
   }

imageToPpm :: Image -> String
imageToPpm (Image w h p) = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ spixels 0
   where
      spixels pos
         | pos == (w*h) = []
         | otherwise = (ppmPixel $ p ! pos) ++ spixels (pos+1)

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
   | otherwise = seq newPixels (Image w h newPixels)
   where
      isx = floor sx
      isy = floor sy
      maxX = w - 1
      maxY = h - 1
      offset = isy * w + isx
      newPixels = pixels // [(offset, newPixel)]
      (oldW, oldS) = (0.0, black) -- pixels ! offset
      newPixel = (oldW + sw, add oldS ss)

makeImage :: Int -> Int -> Image
makeImage w h = Image w h pixels where
    pixels = listArray (0, pxCount - 1) (repeat v)
    pxCount = w * h :: Int
    v = (0.0, black) :: WeightedSpectrum

clamp :: Float -> Int
clamp v = round ( (min 1 (max 0 v)) * 255 )

makePgm :: Int -> Int -> [ Spectrum ] -> String
makePgm width height s = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify s
  where 
    stringify [] = ""
    stringify ((r,g,b):xs) = show (clamp r) ++ " " ++
      show (clamp g) ++ " " ++ show (clamp b) ++ " " ++
      stringify xs
                