module Graphics.Bling.Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam, sampleOneLight, ppScene
   ) where

import Data.Maybe (mapMaybe, isJust, fromJust)
import qualified Data.Vector as V
import Text.PrettyPrint

import Graphics.Bling.Camera
import Graphics.Bling.Light as L
import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum
import Graphics.Bling.Primitive.KdTree

data Scene = Scene {
   scenePrim :: KdTree,
   sceneLights :: V.Vector Light,
   sceneCam :: Camera
   }
   
ppScene :: Scene -> Doc
ppScene (Scene p ls cam) = vcat [
   text "Camera is" <+> ppCamera cam,
   text "bounds" <+> text (show (worldBounds p)),
   text "number of lights" <+> int (V.length ls),
   text "BVH stats" $$ nest 3 (ppKdTree p)
   ]
   
mkScene :: (Primitive a) => [Light] -> [a] -> Camera -> Scene
mkScene l prims cam = Scene (mkKdTree ps) (V.fromList lights) cam where
   lights = l ++ gl
   gl = mapMaybe light ps -- collect the geometric lights
   ps = Prelude.concatMap flatten prims
   
occluded :: Scene -> Ray -> Bool
occluded (Scene p _ _) = intersects p

instance Primitive Scene where
   intersect = intersect.scenePrim
   intersects = intersects.scenePrim
   worldBounds = worldBounds.scenePrim
   flatten = flatten.scenePrim
   light = light.scenePrim
   shadingGeometry = shadingGeometry.scenePrim
   

sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Normal -> Spectrum
sampleLightMis scene (LightSample li wi ray lpdf delta) bsdf wo n
   | lpdf == 0 || isBlack li || isBlack f || occluded scene ray = black
   | delta = sScale (f * li) (absDot wi n / lpdf)
   | otherwise = sScale (f * li) (absDot wi n * weight / lpdf)
   where
         f = evalBsdf bsdf wo wi
         weight = powerHeuristic (1, lpdf) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Normal -> Point -> Spectrum
sampleBsdfMis (Scene sp _ _) l (BsdfSample _ bPdf f wi) n p
   | isBlack f || bPdf == 0 = black
   | isJust lint = maybe black ff $ intLight (fromJust lint)
   | otherwise = scale (le l ray)
   where
         ff l' = if l' == l then scale $ intLe (fromJust lint) (-wi) else black
         lPdf = pdf l p wi
         weight = powerHeuristic (1, bPdf) (1, lPdf)
         scale li = sScale (f * li) (absDot wi n * weight / bPdf)
         ray = Ray p wi epsilon infinity
         lint = sp `intersect` ray

estimateDirect
   :: Scene
   -> Light
   -> Point
   -> Normal
   -> Vector
   -> Bsdf
   -> Sampled Spectrum
{-# INLINE estimateDirect #-}
{-
estimateDirect s l p n wo bsdf = do
   ul <- rnd2D
   let (LightSample li wi ray lpdf _) = sample l p n ul
   let f = (evalBsdf bsdf wo wi)
   if lpdf == 0 || isBlack li || isBlack f || occluded s ray
      then return black
      else return $ sScale (f * li) (absDot wi n / lpdf)
   -}

estimateDirect s l p n wo bsdf = do
   uL <- rnd2D
   uBC <- rnd
   uBD <- rnd2D
   let ls = sampleLightMis s (sample l p n uL) bsdf wo n
   let bs = sampleBsdfMis s l (sampleBsdf bsdf wo uBC uBD) n p
   return $ ls + bs
   
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Float -> Sampled Spectrum
sampleOneLight scene@(Scene _ lights _) p n wo bsdf ulNum
   | lc == 0 = return black
   | lc == 1 = ed (V.head lights)
   | otherwise = do
      ld <- ed $ V.unsafeIndex lights ln
      return $ sScale ld (fromIntegral lc)
      where
            ed l = estimateDirect scene l p n wo bsdf
            lc = V.length lights
            ln = min (floor $ ulNum * fromIntegral lc) (lc - 1)
