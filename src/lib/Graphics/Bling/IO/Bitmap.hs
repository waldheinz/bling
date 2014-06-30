
module Graphics.Bling.IO.Bitmap (
  readTexture, writeRgbe, writePng
  ) where

import qualified Codec.Picture as JP

import           Graphics.Bling.Image
import           Graphics.Bling.Math
import           Graphics.Bling.Spectrum
import           Graphics.Bling.Texture

readTexture :: FilePath -> IO (Either String DiscreteSpectrumMap2d)
readTexture fname = do
  edi <- JP.readImage fname
  case edi of
    Left e   -> return $ Left e
    Right di -> return $ case di of
      JP.ImageRGBF img -> Right $ rgbfToTexMap img
      _                -> Left  $ "can't convert image format to texture"

rgbfToTexMap :: JP.Image JP.PixelRGBF -> DiscreteSpectrumMap2d
rgbfToTexMap img = mkDiscreteTextureMap2d size px where
  size@(w, h) = (JP.imageWidth img, JP.imageHeight img)
  px c = rgbToSpectrumIllum (r, g, b) where
    JP.PixelRGBF r g b = JP.pixelAt img x y
    x = max 0 $ min (w - 1) $ floor $ (1 - u) * fromIntegral w
    y = max 0 $ min (h - 1) $ floor $ (1 - v) * fromIntegral h
    (u, v) = unCartesian c

imageToJp :: Image -> JP.Image JP.PixelRGBF
imageToJp img = JP.generateImage px w h where
  w     = imgW img
  h     = imgH img
  px x y = let (r, g, b) = getPixel img 1 (y * w + x) in JP.PixelRGBF r g b

writeRgbe :: Image -> FilePath -> IO ()
writeRgbe img fname = JP.writeHDR fname $ imageToJp img

writePng :: Image -> FilePath -> IO ()
writePng img fname = JP.savePngImage fname $ JP.ImageRGBF $ imageToJp img
