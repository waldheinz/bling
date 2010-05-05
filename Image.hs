
module Image(
   Image, ImageSample(..),
   mkImage,
   imageWidth, imageHeight, 
   writePpm, addSample) where

import Debug.Trace

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Array.ST
import System.IO

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
   
addPixel :: Image s -> (Int, Int, WeightedSpectrum) -> ST s ()
addPixel (Image w _ p) (x, y, (sw, s)) = do
   osw <- readArray p o'
   writeArray p o' (osw + sw)
   
   ox <- readArray p (o' + 1)
   writeArray p (o' + 1) (ox + sx)
   
   oy <- readArray p (o' + 2)
   writeArray p (o' + 2) (oy + sy)
   
   oz <- readArray p (o' + 3)
   writeArray p (o' + 3) (oz + sz)
   
--   Image w h p
   where
         (sx, sy, sz) = toXyz s
         o' = (x + y*w) * 4

type Filter = ImageSample -> [(Int, Int, WeightedSpectrum)]

boxFilter :: Filter
boxFilter (ImageSample x y ws) = [(floor x, floor y, ws)]

-- | adds an sample to the specified image
addSample :: Image s -> ImageSample -> ST s ()
addSample img smp@(ImageSample sx sy (_, ss))
   | sNaN ss = trace ("skipping NaN sample at (" ++ (show sx) ++ ", " ++ (show sy) ++ ")") (return () )
   | otherwise = sequence_ $ map (addPixel img) pixels
   where
         pixels = boxFilter smp

-- | extracts the pixel at the specified offset from an Image
getPixel :: Image s -> Int -> ST s WeightedSpectrum
getPixel (Image _ _ p) o = do
   w <- readArray p o'
   x <- readArray p (o' + 1)
   y <- readArray p (o' + 2)
   z <- readArray p (o' + 3)
   return $ (w, fromXyz (x, y, z)) where
      o' = o * 4
   
-- | writes an image in ppm format
writePpm :: Image RealWorld -> Handle -> IO ()
writePpm img@(Image w h _) handle = 
   let
       header = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"
       pixel p = stToIO $ (liftM ppmPixel) $ getPixel img p
   in do
      hPutStr handle header
      sequence_ $ map (\p -> pixel p >>= (hPutStr handle)) [0..(w*h-1)]
      
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
