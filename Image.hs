
module Image(makePgm, makeImage) where

import Control.Monad.ST
import Data.Array.Diff

import Color

type WeightedSpectrum = (Float, Spectrum)

data Image = Image {
   imageWidth :: Int,
   imageHeight :: Int,
   imagePixels :: (Array Int WeightedSpectrum)
   }

makeImage :: Int -> Int -> Image
makeImage w h = Image w h pixels where
    pixels = listArray (0, pxCount -1) (repeat v)
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
