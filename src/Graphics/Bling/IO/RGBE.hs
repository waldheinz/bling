
module Graphics.Bling.IO.RGBE (
   -- * RGBE image format support
   ) where
   

import Control.Monad (liftM)
import Data.List (zipWith4)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char as C
import Data.Word

import Debug.Trace

import Graphics.Bling.Spectrum
import Graphics.Bling.Types

data RGBEImage = RGBE
   { rgbeSize     :: ! PixelSize
   , rgbePixels   :: ! (UV.Vector Spectrum)
   }

instance Show RGBEImage where
   show (RGBE size px) = "RGBE Image (size=" ++ (show size) ++ (show (UV.head px)) ++ ")"

type RGBEHeader = (PixelSize)

parseRGBE :: BS.ByteString -> Either String RGBEImage
parseRGBE bs
   | isNothing header = Left "error parsing RGBE header"
   | otherwise = trace ("rest = " ++ (show $ BS.length blah)) $ Right $ RGBE (fromJust header) (UV.fromList pixels)
   where
      (rest, header) = parseRGBEHeader bs
      (blah, pixels) = parseRGBEPixels rest (fromJust header)

parseRGBEPixels :: BS.ByteString -> PixelSize -> (BS.ByteString, [Spectrum])
parseRGBEPixels bs (width, height)
   | width < 8 || width > 0x7fff = readFlatPixels bs (width * height)
   | (r /= 2) || (g /= 2) || ((b .&. 0x80) /= 0) = readFlatPixels bs (width * height)
   | otherwise = readRlePixels bs width height
   where
      (r:g:b:[]) = BS.unpack $ BS.take 3 bs

readRlePixels :: BS.ByteString -> Int -> Int -> (BS.ByteString, [Spectrum])
readRlePixels bs width height = trace (show $ BS.length bs) $ go height (bs, []) where
   go 0 x = x
   go n (bs', lines) = go (n-1) (rest, line ++ lines) where
      (rest, line) = oneLine bs'

   oneLine :: BS.ByteString -> (BS.ByteString, [Spectrum])
   oneLine lbs
      | shiftL ((fromIntegral b) :: Int) 8 .|. fromIntegral e /= width = error "invalid scanline width"
      | otherwise = (rest, zipWith4 rgbeToSpectrum chr chg chb che)
      where
         (bs', chr) = oneChannel bs
         (bs'', chg) = oneChannel bs'
         (bs''', chb) = oneChannel bs''
         (rest, che) = oneChannel bs'''
         (r:g:b:e:_) = BS.unpack $ BS.take 4 bs

   oneChannel :: BS.ByteString -> (BS.ByteString, [Word8])
   oneChannel cbs = go width (cbs, []) where
      go 0 x = x
      go n (cb, xs)
         | b0 > 128 = go (n - fromIntegral b0 + 128) (BS.drop 2 cb, Prelude.replicate ((fromIntegral b0) - 128) b1 ++ xs)
         | otherwise = go (n - fromIntegral b0) (BS.drop (fromIntegral b0) cb, (BS.unpack $ BS.take (fromIntegral b0) cb) ++ xs)
         where
            (b0:b1:_) = BS.unpack $ BS.take 2 bs
   
readFlatPixels :: BS.ByteString -> Int -> (BS.ByteString, [Spectrum])
readFlatPixels bs count = trace ("count=" ++ show count++ " len=" ++ show (BS.length bs)) $ go count (bs, []) where
   go 0 x = x
   go n (bs', ss) = go (n-1) (rest, s:ss) where
      (rest, s) = trace ("bs' len = " ++ (show $ BS.length bs')) $ readFlatPixel bs'

readFlatPixel :: BS.ByteString -> (BS.ByteString, Spectrum)
readFlatPixel bs = (BS.drop 4 bs, rgbeToSpectrum r g b e) where
   (r:g:b:e:_) = BS.unpack $ BS.take 4 bs

rgbeToSpectrum :: Word8 -> Word8 -> Word8 -> Word8 -> Spectrum
rgbeToSpectrum r g b e
   | e == 0 = black
   | otherwise = fromRGB (r', g', b')
   where
      r' = fromIntegral r * f;
      g' = fromIntegral g * f;
      b' = fromIntegral b * f;
      f = ldexp 1 (e - 128 + 8)
      ldexp x exp = x * (2 ** fromIntegral exp)

parseRGBEHeader :: BS.ByteString -> (BS.ByteString, Maybe RGBEHeader)
parseRGBEHeader bs = (BS.tail rest, Just size) where
   (headerLines, pixAndSize) = splitHeader bs
   (sizeStr, rest) = BS.span (/=(fromIntegral $ C.ord '\n')) pixAndSize
   size' = BS.split (BSI.c2w ' ') sizeStr
   size = (Prelude.read $ bsToStr (size' !! 1), Prelude.read $ bsToStr (size' !! 3))

bsToStr :: BS.ByteString -> String
bsToStr bs = map BSI.w2c $ BS.unpack bs

splitHeader :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitHeader bs = go ([], bs) where
   go (hls, rest)
      | BS.null line = (hls, BS.tail rest)
      | otherwise = go ((line:hls), BS.tail rest')
      where
         (line, rest') = BS.span (/=(BSI.c2w '\n')) rest
   