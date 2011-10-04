
module Graphics.Bling.IO.RGBE (
   -- * RGBE image format support
   ) where
   

import Control.Monad (liftM)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Bits ((.&.))
import Data.Char as C
import Data.Word

import Graphics.Bling.Spectrum
import Graphics.Bling.Types

data RGBEImage = RGBE
   { rgbeSize     :: PixelSize
   , rgbePixels   :: [Spectrum]
   }

instance Show RGBEImage where
   show (RGBE size px) = "RGBE Image (size=" ++ (show size) ++ (show (head px)) ++ ")"

type RGBEHeader = (PixelSize)

parseRGBE :: BS.ByteString -> Either String RGBEImage
parseRGBE bs
   | isNothing header = Left "error parsing RGBE header"
--    | isNothing pixels = Left "error parsing pixel data"
   | otherwise = Right $ RGBE (fromJust header) (pixels)
   where
      (rest, header) = parseRGBEHeader bs
      (_, pixels) = parseRGBEPixels rest (fromJust header)

parseRGBEPixels :: BS.ByteString -> PixelSize -> (BS.ByteString, [Spectrum])
parseRGBEPixels bs (width, height)
   | width < 8 || width > 0x7fff = readFlatPixels bs (width * height)
   | (r /= 2) || (g /= 2) || (b .&. 0x80 /= 0) = readFlatPixels bs (width * height)
   | otherwise = undefined -- readRlePixels bs width height
   where
      (r:g:b:[]) = BS.unpack $ BS.take 3 bs

readRlePixels :: BS.ByteString -> Int -> Int -> (BS.ByteString, [Spectrum])
readRlePixels bs width height = go height (bs, []) where
   go 0 x = x
   go n (bs', lines) = go (n-1) (rest, line ++ lines) where
      (rest, line) = oneLine bs'

   oneLine :: BS.ByteString -> (BS.ByteString, [Spectrum])
   oneLine lbs = undefined

readFlatPixels :: BS.ByteString -> Int -> (BS.ByteString, [Spectrum])
readFlatPixels bs count = go count (bs, []) where
   go 0 x = x
   go n (bs, ss) = go (n-1) (rest, s:ss) where
      (rest, s) = readFlatPixel bs

readFlatPixel :: BS.ByteString -> (BS.ByteString, Spectrum)
readFlatPixel bs = (BS.drop 4 bs, rgbeToSpectrum (r, g, b, e)) where
   (r:g:b:e:[]) = BS.unpack $ BS.take 4 bs

rgbeToSpectrum :: (Word8, Word8, Word8, Word8) -> Spectrum
rgbeToSpectrum (r, g, b, e)
   | e == 0 = black
   | otherwise = fromRGB (r', g', b')
   where
      r' = fromIntegral r * f;
      g' = fromIntegral g * f;
      b' = fromIntegral b * f;
      f = ldexp 1 (e - 128 + 8)
      ldexp x exp = x * (2 ** fromIntegral exp)

parseRGBEHeader :: BS.ByteString -> (BS.ByteString, Maybe RGBEHeader)
parseRGBEHeader bs = (rest, Just size) where
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
   