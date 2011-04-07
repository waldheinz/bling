
module Graphics.Bling.IO.MaterialParser (
   defaultMaterial, pMaterial
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Reflection
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.Material.Lafortune
import Graphics.Bling.Material.Matte
import Graphics.Bling.Material.Plastic
import Graphics.Bling.Material.Specular

defaultMaterial :: Material
defaultMaterial = mkMatte (constant $ fromRGB (0.9, 0.9, 0.9))

--
-- parsing materials
--

pMaterial :: JobParser ()
pMaterial = do
   _ <- try (string "beginMaterial")
   ws >> string "type" >> ws
   t <- many alphaNum
   m <- case t of
      "measured" -> do
         m <- pMeasuredMaterial
         return (measuredMaterial m)

      "plastic" -> pPlasticMaterial
      "matte" -> pMatteMaterial
      "mirror" -> pMirrorMaterial

      _ -> fail ("unknown material type " ++ t)

   _ <- ws >> string "endMaterial"
   s <- getState
   setState s { material = m }

pMirrorMaterial :: JobParser Material
pMirrorMaterial = do
   r <- ws >> pSpectrum
   return (mirrorMaterial r)

pMatteMaterial :: JobParser Material
pMatteMaterial = do
   kd <- pTexture "kd"
   return (mkMatte kd)

pPlasticMaterial :: JobParser Material
pPlasticMaterial = do
   kd <- pTexture "kd"
   ks <- pTexture "ks"
   rough <- ws >> namedFloat "rough"
   return (plasticMaterial kd ks rough)

pTexture :: String -> JobParser SpectrumTexture
pTexture n = do
   ws >> string "beginTexture" >> ws >> string n >> ws >> string "type" >> ws
   tp <- many alphaNum
   ws
   tx <- case tp of
      "constant" -> do
         s <- pSpectrum
         return (constant s)
      _ -> fail ("unknown texture type " ++ tp)
   _ <- ws >> string "endTexture"
   return tx

pMeasuredMaterial :: JobParser Measured
pMeasuredMaterial = do
   _ <- string "name" >> ws
   n <- many alphaNum
   return (read n)
   