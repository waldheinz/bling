
module Graphics.Bling.IO.Bitmap (
  readTexture, writeRgbe, writePng
  ) where

import qualified Codec.Picture as JP

import           Graphics.Bling.Image
import           Graphics.Bling.Texture


readTexture :: FilePath -> IO (Either String DiscreteSpectrumMap2d)
readTexture fname = do
  edi <- JP.readImage fname
  case edi of
    Left e   -> return $ Left e
    Right di -> case di of
      _ -> return $ Left $ "can't convert image format to texture"

imageToJp :: Image -> JP.Image JP.PixelRGBF
imageToJp img = JP.generateImage px w h where
  w     = imgW img
  h     = imgH img
  px x y = let (r, g, b) = getPixel img 1 (y * w + x) in JP.PixelRGBF r g b

writeRgbe :: Image -> FilePath -> IO ()
writeRgbe img fname = JP.writeHDR fname $ imageToJp img

writePng :: Image -> FilePath -> IO ()
writePng img fname = JP.savePngImage fname $ JP.ImageRGBF $ imageToJp img
