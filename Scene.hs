module Scene where

import Control.Monad

import Color
import Light
import Math
import Primitive
import Random
import Transport

data Scene = Scene {
   scenePrimitive :: Primitive,
   sceneLights :: [Light]
   }

occluded :: Scene -> Ray -> Bool
occluded (Scene p _) = primIntersects p
   
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

sampleBsdfMis :: Scene -> LightSample -> Spectrum
sampleBsdfMis scene (LightSample li wi ray pdf deltaLight)
   | deltaLight = black
   | otherwise = undefined
      
-- | samples all lights by sampling individual lights and summing up the results
sampleAllLights :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleAllLights scene p n wo bsdf = undefined
--   return (foldl (liftM2 (+)) (return black) spectri) -- sum up contributions
--  where
--    spectri = map (\l -> evalLight scene p n l wo bsdf) lights
--    lights = sceneLights scene

-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleOneLight (Scene _ []) _ _ _ _ = return black -- no light sources -> no light
sampleOneLight scene@(Scene _ (l:[])) p n wo bsdf = undefined -- evalLight scene p n l wo bsdf -- eval the only light source
sampleOneLight scene@(Scene _ lights) p n wo bsdf = undefined
--  lightNumF <-rndR (0, fromIntegral lightCount)
--  lightNum <- return $ floor lightNumF
--  y <- evalLight scene p n (lights !! lightNum) wo bsdf
--  return $! scale y
--  where
--    lightCount = length lights
--    scale = (\y -> sScale y (fromIntegral lightCount))
