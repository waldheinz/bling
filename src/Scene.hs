module Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam,
   Integrator, sampleOneLight, sampleAllLights, ppScene
   ) where

import Control.Monad
import Data.Array.Unboxed
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Text.PrettyPrint

import Bvh
import Camera
import Light as L
import Math
import Montecarlo
import Primitive
import Random
import Spectrum
import Transport

data Scene = Scene {
   scenePrim :: Bvh,
   sceneLights :: Array Int Light,
   sceneCam :: Camera
   }
   
ppScene :: Scene -> Doc
ppScene (Scene p ls _) = vcat [
   text "bounds" <+> text (show (worldBounds p)),
   text "number of lights" <+> int (rangeSize (bounds ls)),
   text "BVH stats" $$ nest 3 (ppBvh p)
   ]
   
mkScene :: (Primitive a) => [Light] -> [a] -> Camera -> Scene
mkScene l prims cam = Scene (mkBvh ps) la cam where
   la = listArray (0, length lights - 1) lights
   lights = l ++ gl
   gl = mapMaybe light ps -- collect the geometric lights
   ps = concatMap flatten prims
   
occluded :: Scene -> Ray -> Bool
occluded (Scene p _ _) = intersects p

type Integrator = Scene -> Ray -> Rand WeightedSpectrum

evalLight :: Scene -> Point -> Normal -> Light -> Vector -> Bsdf -> Rand2D -> Spectrum
evalLight scene p n l wo bsdf us = evalSample scene s wo bsdf p n where
   s = L.sample l p n us
   
evalSample :: Scene -> LightSample -> Vector -> Bsdf -> Point -> Normal -> Spectrum
evalSample scene s wo bsdf _ n
   | isBlack li || isBlack f = black
   | occluded scene (testRay s) = black
   | otherwise = sScale (f * li) $ absDot wi n / lPdf
   where
         lPdf = lightSamplePdf s
         li = de s
         wi = lightSampleWi s
         f = evalBsdf bsdf wo wi
   
sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Normal -> Spectrum
sampleLightMis scene (LightSample li wi ray p deltaLight) bsdf wo n
   | p == infinity || isBlack li || isBlack f || occluded scene ray = black
   | deltaLight = sScale (f * li) (absDot wi n / p)
   | otherwise = sScale (f * li) (absDot wi n * weight / p)
   where
         f = evalBsdf bsdf wo wi
         weight = powerHeuristic (1, p) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Normal -> Point -> Spectrum
sampleBsdfMis (Scene sp _ _) l (BsdfSample _ bPdf f wi) n p
   | isBlack f || bPdf == 0 = black
   | isJust lint = scale $ intLe (fromJust lint) (neg wi) -- TODO: need to check if the "right" light was hit
   | otherwise = scale (le l ray)
   where
         lPdf = pdf l p wi
         weight = powerHeuristic (1, bPdf) (1, lPdf)
         scale li = sScale (f * li) (absDot wi n * weight / bPdf)
         ray = Ray p wi epsilon infinity
         lint = sp `intersect` ray
         
-- | samples all lights by sampling individual lights and summing up the results
sampleAllLights :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleAllLights scene p n wo bsdf = undefined

estimateDirect :: Scene -> Light -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
estimateDirect s l p n wo bsdf = 
   let lSmp = sample l p n
       bSmp = sampleBsdf bsdf wo
   in do
      uL <- rnd2D
      uBC <- rnd
      uBD <- rnd2D
      let ls = sampleLightMis s (lSmp uL) bsdf wo n
      let bs = sampleBsdfMis s l (bSmp uBC uBD) n p
      return (ls + bs)
   
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Float -> Rand Spectrum
sampleOneLight scene@(Scene _ lights _) p n wo bsdf ulNum
   | lightCount == 0 = return black
   | lightCount == 1 = estimateDirect scene (lights ! 0)  p n wo bsdf
   | otherwise = liftM scale (estimateDirect scene (lights ! lightNum) p n wo bsdf)
      where
            lightCount = snd (bounds lights) + 1
            lightNum = floor $ ulNum * fromIntegral lightCount
            scale y = sScale y (fromIntegral lightCount)
