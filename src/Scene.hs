module Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam,
   Integrator, sampleOneLight, sampleAllLights, ppScene
   ) where

import Control.Monad
import Data.Array.Unboxed
import Data.Maybe (isJust, fromJust, catMaybes)
import Debug.Trace
import Text.PrettyPrint

import AABB
import Bvh
import Camera
import Image
import Light
import Math
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
ppScene (Scene p ls c) = vcat [
   text "bounds" <+> text (show (primWorldBounds p)),
   text "number of lights" <+> (int (rangeSize (bounds ls))),
   text "BVH stats" $$ nest 3 (ppBvh p)
   ]
   
mkScene :: (Prim a) => [Light] -> [a] -> Camera -> Scene
mkScene l prims cam = Scene (mkBvh ps) la cam where
   la = listArray (0, length lights - 1) lights
   lights = l ++ gl
   gl = catMaybes $ map primLight ps -- collect the geometric lights
   ps = concatMap primFlatten prims
   
occluded :: Scene -> Ray -> Bool
occluded (Scene p _ _) = primIntersects p

type Integrator = Scene -> Ray -> Rand WeightedSpectrum
   
evalLight :: Scene -> Point -> Normal -> Light -> Vector -> Bsdf -> Rand2D -> Spectrum
evalLight scene p n light wo bsdf us = (evalSample scene sample wo bsdf p n) where
   sample = lightSample light p n us
   
evalSample :: Scene -> LightSample -> Vector -> Bsdf -> Point -> Normal -> Spectrum
evalSample scene sample wo bsdf _ n
   | isBlack li || isBlack f = black
   | occluded scene (testRay sample) = black
   | otherwise = sScale (f * li) $ (absDot wi n) / lPdf
   where
         lPdf = lightSamplePdf sample
         li = de sample
         wi = lightSampleWi sample
         f = evalBsdf bsdf wo wi
   
sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Normal -> Spectrum
sampleLightMis scene (LightSample li wi ray pdf deltaLight) bsdf wo n
   | (pdf == infinity) || (isBlack li) || (isBlack f) || (occluded scene ray) = black
   | deltaLight = sScale (f * li) ((absDot wi n) / pdf)
   | otherwise = sScale (f * li) ((absDot wi n) * weight / pdf)
   where
         f = evalBsdf bsdf wo wi
         weight = powerHeuristic (1, pdf) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Normal -> Point -> Spectrum
sampleBsdfMis (Scene sp _ _) light (BsdfSample _ bPdf f wi) n p
   | (isBlack f) || (bPdf == 0) = black
   | isJust lint = scale $ intLe (fromJust lint) (neg wi) -- TODO: need to check if the "right" light was hit
   | otherwise = scale (lightEmittance light ray)
   where
         lPdf = lightPdf light p n wi
         weight = powerHeuristic (1, bPdf) (1, lPdf)
         scale = (\li -> sScale (f * li) ((absDot wi n) * weight / bPdf))
         ray = Ray p wi epsilon infinity
         lint = primIntersect sp ray
         
-- | samples all lights by sampling individual lights and summing up the results
sampleAllLights :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleAllLights scene p n wo bsdf = undefined

estimateDirect :: Scene -> Light -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
estimateDirect s l p n wo bsdf = 
   let lSmp = lightSample l p n
       bSmp = sampleBsdf bsdf wo
   in do
   uL <- rnd2D
   uBC <- rnd
   uBD <- rnd2D
   return $ (sampleLightMis s (lSmp uL) bsdf wo n) + (sampleBsdfMis s l (bSmp uBC uBD) n p)
   
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Float -> Rand Spectrum
sampleOneLight scene@(Scene _ lights _) p n wo bsdf ulNum
   | lightCount == 0 = return $ black
   | lightCount == 1 = estimateDirect scene (lights ! 0)  p n wo bsdf
   | otherwise = (liftM scale) (estimateDirect scene (lights ! lightNum) p n wo bsdf)
      where
            lightCount = snd (bounds lights) + 1
            lightNum = floor $ ulNum * (fromIntegral lightCount)
            scale = (\y -> sScale y (fromIntegral lightCount))
