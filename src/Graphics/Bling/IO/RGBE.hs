
module Graphics.Bling.IO.RGBE (
   -- * RGBE image format support
   ) where
   

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as V 
import Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char as C

import Graphics.Bling.Types

type RGBEHeader = (PixelSize)



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
   