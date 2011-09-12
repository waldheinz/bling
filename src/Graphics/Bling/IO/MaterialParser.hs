
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
import Graphics.Bling.Material.Substrate

defaultMaterial :: Material
defaultMaterial = mkMatte (constant $ fromRGB (0.9, 0.9, 0.9)) (constant 0)

--
-- parsing materials
--

pMaterial :: JobParser ()
pMaterial = (flip namedBlock) "material" $ do
   m <- pMaterial'
   s <- getState
   setState s { material = m }

pMaterial' :: JobParser Material
pMaterial' = do
   t <- many alphaNum
   ws
   case t of
      "blackbody"    -> return blackBodyMaterial
      "bumpMap"      -> pBumpMap
      "glass"        -> pGlass
      "measured"     -> pMeasuredMaterial
      "metal"        -> pMetalMaterial
      "shinyMetal"   -> pShinyMetal
      "substrate"    -> pSubstrateMaterial
      "plastic"      -> pPlasticMaterial
      "matte"        -> pMatteMaterial
      "mirror"       -> pMirrorMaterial
      _              -> fail ("unknown material type " ++ t)

pBumpMap :: JobParser Material
pBumpMap = do
   d <- pScalarTexture "bump"
   m <- ws >> pMaterial'
   return $ bumpMapped d m

pSubstrateMaterial :: JobParser Material
pSubstrateMaterial = do
   kd <- pSpectrumTexture "kd"
   ks <- ws >> pSpectrumTexture "ks"
   urough <- ws >> pScalarTexture "urough"
   vrough <- ws >> pScalarTexture "vrough"
   return $ mkSubstrate kd ks urough vrough

pGlass :: JobParser Material
pGlass = do
   ior <- namedFloat "ior"
   s <- ws >> pSpectrumTexture "r"
   return $ glassMaterial ior s

pMetalMaterial :: JobParser Material
pMetalMaterial = do
   eta <- pSpectrumTexture "eta"
   k <- ws >> pSpectrumTexture "k"
   rough <- ws >> pScalarTexture "rough"
   return (mkMetal eta k rough)

pShinyMetal :: JobParser Material
pShinyMetal = do
   kr <- pSpectrumTexture "kr"
   ks <- ws >> pSpectrumTexture "ks"
   rough <- ws >> pScalarTexture "rough"
   return $ mkShinyMetal kr ks rough


pMirrorMaterial :: JobParser Material
pMirrorMaterial = do
   r <- pSpectrum
   return (mirrorMaterial r)

pMatteMaterial :: JobParser Material
pMatteMaterial = do
   kd <- pSpectrumTexture "kd"
   sig <- ws >> pScalarTexture "sigma"
   return (mkMatte kd sig)
   
pMeasuredMaterial :: JobParser Material
pMeasuredMaterial = do
   _ <- string "name" >> ws
   n <- many alphaNum
   return $ measuredMaterial (read n)
   
pPlasticMaterial :: JobParser Material
pPlasticMaterial = do
   kd <- pSpectrumTexture "kd"
   ks <- ws >> pSpectrumTexture "ks"
   rough <- ws >> pScalarTexture "rough"
   return (mkPlastic kd ks rough)

pScalarTexture :: String -> JobParser ScalarTexture
pScalarTexture = namedBlock $ do
   tp <- many alphaNum
   ws >> case tp of
      "constant" -> do
         v <- flt
         return (constant v)
      
      "scale" -> do
         s <- flt
         t <- ws >> pScalarTexture "texture"
         return $ scaleTexture s t
         
      "perlin" -> return noiseTexture
      
      _ -> fail ("unknown texture type " ++ tp)

pSpectrumTexture :: String -> JobParser SpectrumTexture
pSpectrumTexture = namedBlock $ do
   tp <- many alphaNum
   ws >> case tp of
      "constant" -> do
         s <- pSpectrum
         return (constant s)
      
      "checker" -> do
         s <- pVec
         t1 <- ws >> pSpectrumTexture "tex1"
         t2 <- ws >> pSpectrumTexture "tex2"
         return (checkerBoard s t1 t2)

      "graphPaper" -> do
         s <- flt
         t1 <- ws >> pSpectrumTexture "tex1"
         t2 <- ws >> pSpectrumTexture "tex2"
         return (graphPaper s t1 t2)
         
      "wood" -> return woodTexture
         
      _ -> fail ("unknown texture type " ++ tp)

