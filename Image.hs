
module Image(makePgm, makeImage) where

import Control.Monad.ST
import Data.Array.ST

import Color

type WeightedSpectrum = (Float, Spectrum)

data Image = Image {
   imageWidth :: Int,
   imageHeight :: Int
 --  imagePixels :: (STArray s Int WeightedSpectrum)
   } deriving Show

makeImage w h = do
   arr <- newArray (0, pxCount) v
   return (w, h, arr) where
      pxCount = w * h :: Int
      v = (0.0, black) :: WeightedSpectrum
         

-- | creates an all-black image of the specified width and height
--makeImage :: Int -> Int -> Image
--makeImage w h = Image w h pixels where
--   pxCount = w * h
--   pixels = array (0, pxCount) $ [(i, (0, black)) | i <- [0..pxCount]]
   
clamp :: Float -> Int
clamp v = round ( (min 1 (max 0 v)) * 255 )

makePgm :: Int -> Int -> [ Spectrum ] -> String
makePgm width height s = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify s
  where 
    stringify [] = ""
    stringify ((r,g,b):xs) = show (clamp r) ++ " " ++
      show (clamp g) ++ " " ++ show (clamp b) ++ " " ++
      stringify xs
