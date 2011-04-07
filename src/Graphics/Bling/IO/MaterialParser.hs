
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
import Graphics.Bling.Material.Metal
import Graphics.Bling.Material.Plastic
import Graphics.Bling.Material.Specular

defaultMaterial :: Material
defaultMaterial = mkMatte (constant $ fromRGB (0.9, 0.9, 0.9)) (constant 0)

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
      "metal" -> pMetalMaterial
      "plastic" -> pPlasticMaterial
      "matte" -> pMatteMaterial
      "mirror" -> pMirrorMaterial

      _ -> fail ("unknown material type " ++ t)

   _ <- ws >> string "endMaterial"
   s <- getState
   setState s { material = m }

pMetalMaterial :: JobParser Material
pMetalMaterial = do
   eta <- pSpectrumTexture "eta"
   k <- pSpectrumTexture "k"
   rough <- pScalarTexture "rough"
   return (mkMetal eta k rough)

pMirrorMaterial :: JobParser Material
pMirrorMaterial = do
   r <- ws >> pSpectrum
   return (mirrorMaterial r)

pMatteMaterial :: JobParser Material
pMatteMaterial = do
   kd <- pSpectrumTexture "kd"
   sig <- pScalarTexture "sigma"
   return (mkMatte kd sig)
   
pPlasticMaterial :: JobParser Material
pPlasticMaterial = do
   kd <- pSpectrumTexture "kd"
   ks <- pSpectrumTexture "ks"
   rough <- pScalarTexture "rough"
   return (mkPlastic kd ks rough)

pScalarTexture :: String -> JobParser ScalarTexture
pScalarTexture n = do
   _ <- ws >> string n >> ws >> char '{' >> ws
   tp <- many alphaNum
   ws
   tx <- case tp of
      "constant" -> do
         v <- flt
         return (constant v)
         
      _ -> fail ("unknown texture type " ++ tp)

   _ <- ws >> char '}'
   return tx
   
pSpectrumTexture :: String -> JobParser SpectrumTexture
pSpectrumTexture n = do
   ws >> string n >> ws >> char '{' >> ws
   tp <- many alphaNum
   ws
   tx <- case tp of
      "constant" -> do
         s <- pSpectrum
         return (constant s)
      
      "checker" -> do
         s <- pVec
         t1 <- pSpectrumTexture "tex1"
         t2 <- pSpectrumTexture "tex2"
         return (checkerBoard s t1 t2)

      "graphPaper" -> do
         s <- flt
         t1 <- pSpectrumTexture "tex1"
         t2 <- pSpectrumTexture "tex2"
         return (graphPaper s t1 t2)
         
      _ -> fail ("unknown texture type " ++ tp)
   _ <- ws >> char '}'
   return tx

pMeasuredMaterial :: JobParser Measured
pMeasuredMaterial = do
   _ <- string "name" >> ws
   n <- many alphaNum
   return (read n)
   