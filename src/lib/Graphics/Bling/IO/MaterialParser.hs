
module Graphics.Bling.IO.MaterialParser (
   defaultMaterial, pMaterial, pMaterial',
   
   pDiscSpectrumMap2d, namedDiscSpectrumMap2d,
   pScalarMap2d
   
   ) where

import Control.Applicative
import Control.Monad.IO.Class ( liftIO )
import Text.ParserCombinators.Parsec

import Graphics.Bling.Reflection
import Graphics.Bling.SunSky
import Graphics.Bling.Texture
import Graphics.Bling.IO.Bitmap
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.TransformParser
import Graphics.Bling.Material

defaultMaterial :: Material
defaultMaterial = mkMatte (constant $ rgbToSpectrumRefl (0.9, 0.9, 0.9)) (constant 0)

pMaterial :: JobParser ()
pMaterial = pBlock $ do
   m <- pMaterial'
   s <- getState
   setState s { material = m }

pMaterial' :: JobParser Material
pMaterial' = pString >>= \t -> case t of
   "blackbody"    -> return blackBodyMaterial
   "bumpMap"      -> pBumpMap
   "glass"        -> pGlass
   "metal"        -> pMetalMaterial
   "shinyMetal"   -> pShinyMetal
   "substrate"    -> pSubstrateMaterial
   "plastic"      -> pPlasticMaterial
   "matte"        -> pMatteMaterial
   "transMatte"   -> pMatteTranslucent
   "mirror"       -> pMirrorMaterial
   _              -> fail ("unknown material type " ++ t)

pBumpMap :: JobParser Material
pBumpMap = do
   d <- pScalarTexture "bump"
   m <- pMaterial'
   return $! bumpMapped d m

pSubstrateMaterial :: JobParser Material
pSubstrateMaterial = do
   kd <- pSpectrumTexture "kd"
   ks <- pSpectrumTexture "ks"
   ka <- pSpectrumTexture "ka"
   urough <- pScalarTexture "urough"
   vrough <- pScalarTexture "vrough"
   depth <- pScalarTexture "depth"
   return $! mkSubstrate kd ks ka urough vrough depth

pGlass :: JobParser Material
pGlass = do
   ior <- pScalarTexture "ior"
   rt <- pSpectrumTexture "kr"
   tt <- pSpectrumTexture "kt"
   return $! glassMaterial ior rt tt

pMetalMaterial :: JobParser Material
pMetalMaterial = do
   eta <- pSpectrumTexture "eta"
   k <- pSpectrumTexture "k"
   rough <- pScalarTexture "rough"
   return $! mkMetal eta k rough

pShinyMetal :: JobParser Material
pShinyMetal = do
   kr <- pSpectrumTexture "kr"
   ks <- pSpectrumTexture "ks"
   rough <- pScalarTexture "rough"
   return $! mkShinyMetal kr ks rough

pMirrorMaterial :: JobParser Material
pMirrorMaterial = do
   kr <- pSpectrumTexture "kr"
   return $! mirrorMaterial kr

pMatteMaterial :: JobParser Material
pMatteMaterial = do
   kd <- pSpectrumTexture "kd"
   sig <- pScalarTexture "sigma"
   return $! mkMatte kd sig
   
pMatteTranslucent :: JobParser Material
pMatteTranslucent = do
   kr <- pSpectrumTexture "kr"
   kt <- pSpectrumTexture "kt"
   ks <- pScalarTexture "ks"
   return $! translucentMatte kr kt ks

pPlasticMaterial :: JobParser Material
pPlasticMaterial = do
   kd <- pSpectrumTexture "kd"
   ks <- pSpectrumTexture "ks"
   rough <- pScalarTexture "rough"
   return $! mkPlastic kd ks rough

pImageScalar :: JobParser ScalarTexture
pImageScalar = do
   string "file" >> ws
   texmap <- readImageScalarMap <$> (pQString >>= readFileBS')
   
   case texmap of
      Left err -> fail err
      Right sm -> imageTexture sm <$> pTextureMapping2d "map"

pScalarTexture :: String -> JobParser ScalarTexture
pScalarTexture = namedBlock $
   pString >>= \tp -> case tp of
      "constant" -> do
         v <- flt
         return $! constant v

      "image" -> pBlock pImageScalar
   
      "cellNoise" -> do
         d <- pString >>= \dn -> return $ case dn of
            "euclidian"    -> euclidianDist
            "euclidian2"   -> sqEuclidianDist
            "manhattan"    -> manhattanDist
            "chebyshev"    -> chebyshevDist
            _              -> fail $ "unknown distance function " ++ dn
            
         m <- pTextureMapping3d "map"
         return $! cellNoise d m

      "fbm" -> do
         f <- pFbmMap
         m <- pTextureMapping3d "map"
         return $! texMap3dToTexture f m
      
      "perlin" -> do
         m <- pTextureMapping3d "map"
         return $! noiseTexture m

      "crystal" -> do
         o <- namedInt "octaves"
         m <- pTextureMapping2d "map"
         return $! quasiCrystal o m
      
      "scale" -> do
         a <- flt
         s <- flt
         t <- pScalarTexture "tex"
         return $! scaleTexture a s t
      
      _ -> fail ("unknown texture type " ++ tp)

pFbmMap :: JobParser ScalarMap3d
pFbmMap = fbm <$> namedInt "octaves" <*> namedFloat "omega"

pTextureMapping2d :: String -> JobParser TextureMapping2d
pTextureMapping2d = namedBlock $ do
   n <- pString
   case n of
      "planar" -> do
         vu <- pVec
         vv <- pVec
         ou <- flt
         ov <- flt
         return $ planarMapping (vu, vv) (ou, ov)
         
      "uv"   -> do
         su <- flt
         sv <- flt
         ou <- flt
         ov <- flt
         return $ uvMapping (su, sv) (ou, ov)
         
      _      -> fail $ "unknown 2d mapping " ++ n

pTextureMapping3d :: String -> JobParser TextureMapping3d
pTextureMapping3d = namedBlock $
   pString >>= \ mt -> case mt of
              "identity" -> do
                 t <- pTransform
                 return $ identityMapping3d t
                 
              _ -> fail $ "unknown 3d mapping " ++ mt

pImageTexture :: JobParser SpectrumTexture
pImageTexture = do
   string "file" >> ws
   texmap <- readImageTextureMap <$> (pQString >>= readFileBS')
      
   case texmap of
      Left err -> fail err
      Right sm -> imageTexture sm <$> pTextureMapping2d "map"

pSpectrumTexture :: String -> JobParser SpectrumTexture
pSpectrumTexture = namedBlock $ 
   pString >>= \tp -> case tp of
      "blend" -> spectrumBlend <$> pSpectrumTexture "tex1" <*> pSpectrumTexture "tex2" <*> pScalarTexture "f"
      
      "image" -> pBlock pImageTexture
      
      "constant" -> do
         s <- pSpectrum
         return $! constant s
      
      "checker" -> checkerBoard <$> pVec <*> pSpectrumTexture "tex1" <*> pSpectrumTexture "tex2"
      
      "gradient" -> do
         f <- pScalarTexture "f"
         steps <- (flip namedBlock) "steps" $ (flip sepBy) (char ',' >> Graphics.Bling.IO.ParserCore.optional ws) $ do
            pos <- flt
            col <- pSpectrum
            return $! (pos, col)
            
         return $! gradient (mkGradient steps) f
         
      "graphPaper" -> graphPaper
         <$> flt
         <*> pTextureMapping2d "map"
         <*> pSpectrumTexture "tex1"
         <*> pSpectrumTexture "tex2"
         
      _ -> fail ("unknown texture type " ++ tp)

--------------------------------------------------------------------------------
-- Texture Maps
--------------------------------------------------------------------------------

pScalarMap2d :: JobParser ScalarMap2d
pScalarMap2d = pBlock $ do
   pString >>= \tp -> case tp of
      "fbm" -> do
         z <- flt
         m <- pFbmMap
         return $! texMap3dTo2d m z
      
      "scale" -> do
         f <- flt
         m <- pScalarMap2d
         return $! (\x -> f * m x)
      
      x -> fail $ "unknown scalar map type" ++ x
      
namedDiscSpectrumMap2d :: String -> JobParser DiscreteSpectrumMap2d
namedDiscSpectrumMap2d n = string n >> ws >> pDiscSpectrumMap2d

pDiscSpectrumMap2d :: JobParser DiscreteSpectrumMap2d
pDiscSpectrumMap2d = pBlock $ do
   tp <- pString
   case tp of
      "constant" -> do
         s <- pSpectrum
         return $ constSpectrumMap2d s

      "file" -> do
         eimg <- pQString >>= liftIO . readTexture
         case eimg of
            Left e  -> fail e
            Right x -> return x

      "sunSky" -> do
         east <- namedVector "east"
         sunDir <- namedVector "sunDir"
         turb <- namedFloat "turbidity"
         return $ mkSunSkyLight east sunDir turb
                      
      _ -> fail $ "unknown map type " ++ tp
   
