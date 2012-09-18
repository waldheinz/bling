
module Graphics.Bling.IO.RGBE (

   -- * RGBE image format support

   RGBEImage, parseRGBE, rgbeToTextureMap, writeRgbe
   
   ) where
   
import Data.List (zipWith4)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char as C
import Data.Word
import System.IO

import Graphics.Bling.Image
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture
import Graphics.Bling.Types

data RGBEImage = RGBE
   { _rgbeSize    :: {-# UNPACK #-} ! PixelSize
   , _rgbePixels  :: ! (UV.Vector Spectrum)
   }

instance Show RGBEImage where
   show (RGBE size px) = "RGBE Image (size=" ++ (show size) ++ (show px) ++ ")"

rgbeToTextureMap :: RGBEImage -> SpectrumMap
rgbeToTextureMap (RGBE size@(w, h) px) = {-# SCC "rgbeToTextureMap" #-} mkTextureMap size eval where
   eval cs = eval' $ unCartesian cs
   eval' (u, v) = px UV.! o where
      o = max 0 $ min (UV.length px - 1) $ y * w + x
      x = floor $ u * fromIntegral w
      y = floor $ v * fromIntegral h
      
type RGBEHeader = (PixelSize)

parseRGBE :: BS.ByteString -> Either String RGBEImage
parseRGBE bs
   | isNothing header = {-# SCC "parseRGBE" #-} Left "error parsing RGBE header"
   | otherwise = {-# SCC "parseRGBE" #-} Right $ RGBE (fromJust header) (UV.fromList (concat pixels))
   where
      (rest, header) = parseRGBEHeader bs
      (_, pixels) = parseRGBEPixels rest (fromJust header)

parseRGBEPixels :: BS.ByteString -> PixelSize -> (BS.ByteString, [[Spectrum]])
parseRGBEPixels bs (width, height)
   | width < 8 || width > 0x7fff = readFlatPixels bs (width * height)
   | (r /= 2) || (g /= 2) || ((b .&. 0x80) /= 0) = readFlatPixels bs (width * height)
   | otherwise = readRlePixels bs width height
   where
      (r:g:b:[]) = BS.unpack $ BS.take 3 bs

readRlePixels :: BS.ByteString -> Int -> Int -> (BS.ByteString, [[Spectrum]])
readRlePixels bs width height = go height (bs, []) where
   go 0 x = x
   go n (bs', ls) = go (n-1) (rest, line : ls) where
      (rest, line) = oneLine bs'
   
   oneLine :: BS.ByteString -> (BS.ByteString, [Spectrum])
   oneLine lbs
      | width' /= width = error "invalid scanline width"
      | otherwise = (lrest, zipWith4 rgbeToSpectrum chR chG chB chE)
      where
         width' = shiftL ((fromIntegral b) :: Int) 8 .|. fromIntegral e
         (lbs', chR) = oneChannel $ BS.drop 4 lbs
         (lbs'', chG) = oneChannel lbs'
         (lbs''', chB) = oneChannel lbs''
         (lrest, chE) = oneChannel lbs'''
         (_:_:b:e:_) = BS.unpack $ BS.take 4 lbs

   oneChannel :: BS.ByteString -> (BS.ByteString, [Word8])
   oneChannel cbs = goc width (cbs, []) where
      goc 0 x = x
      goc n (cb, _)
         | n < 0 = error "ewww"
         | b0 == 0 = error "bad scanline data"
         | isRun = (cb', replicate runlen b1 ++ xs')
         | otherwise = (cb', (BS.unpack $ BS.take (fromIntegral b0) (BS.drop 1 cb)) ++ xs')
         where
            (cb', xs') = goc (n - lHere) (BS.drop cHere cb, [])
            lHere = if isRun then runlen else fromIntegral b0
            cHere = if isRun then 2 else fromIntegral b0 + 1
            isRun = b0 > 128
            (b0:b1:_) = BS.unpack $ BS.take 2 cb
            runlen = fromIntegral b0 - 128
            
   
readFlatPixels :: BS.ByteString -> Int -> (BS.ByteString, [[Spectrum]])
readFlatPixels bs count = go count (bs, []) where
   go 0 x = x
   go n (bs', ss) = go (n-1) (rest, s:ss) where
      (rest, s) = readFlatPixel bs'

readFlatPixel :: BS.ByteString -> (BS.ByteString, [Spectrum])
readFlatPixel bs = (BS.drop 4 bs, [rgbeToSpectrum r g b e]) where
   (r:g:b:e:_) = BS.unpack $ BS.take 4 bs

rgbeToSpectrum :: Word8 -> Word8 -> Word8 -> Word8 -> Spectrum
rgbeToSpectrum r g b e = rgbToSpectrumIllum (r', g', b') where
   r' = fromIntegral r * f;
   g' = fromIntegral g * f;
   b' = fromIntegral b * f;
   f = ldexp 1 ((fromIntegral e :: Int) - (128 + 8))
   ldexp x ex = x * (2 ** fromIntegral ex)

parseRGBEHeader :: BS.ByteString -> (BS.ByteString, Maybe RGBEHeader)
parseRGBEHeader bs = (BS.tail rest, Just size) where
   (_, pixAndSize) = splitHeader bs
   (sizeStr, rest) = BS.span (/=(fromIntegral $ C.ord '\n')) pixAndSize
   size' = BS.split (BSI.c2w ' ') sizeStr
   size = (Prelude.read $ bsToStr (size' !! 3), Prelude.read $ bsToStr (size' !! 1))

bsToStr :: BS.ByteString -> String
bsToStr bs = map BSI.w2c $ BS.unpack bs

splitHeader :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitHeader bs = go ([], bs) where
   go (hls, rest)
      | BS.null line = (hls, BS.tail rest)
      | otherwise = go ((line:hls), BS.tail rest')
      where
         (line, rest') = BS.span (/=(BSI.c2w '\n')) rest
   

--------------------------------------------------------------------------------
-- writing
--------------------------------------------------------------------------------

frexp :: Float -> (Float, Int)
frexp x
   | isNaN x = error "NaN given to frexp"
   | isInfinite x = error "infinity given to frexp"
   | otherwise  = frexp' (x, 0) where
      frexp' (s, e)
         | s >= 1.0 = frexp' (s / 2, e + 1)
         | s < 0.5 = frexp' (s * 2, e - 1)
         | otherwise = (s, e)

writeRgbe :: Image -> Float -> Handle -> IO ()
writeRgbe img spw hnd =
   let header = "#?RGBE\nFORMAT=32-bit_rgbe\n\n-Y " ++ show h ++ " +X " ++ show w ++ "\n"
       (w, h) = (imgW img, imgH img)
       pixel :: Int -> IO BS.ByteString
       pixel p = return $ toRgbe $ getPixel img spw p

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
         v'' = v' * 256 / v
         
