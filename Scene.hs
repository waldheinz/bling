module Scene where

import Control.Monad

import Color
import Light
import Math
import Primitive
import Random
import Transport

data Scene = Scene {
   scenePrimitive :: AnyPrimitive,
   sceneLights :: [Light]
   }

instance Primitive Scene where
   primIntersect (Scene p _) = primIntersect p
   primIntersects (Scene p _) = primIntersects p
   primMaterial (Scene p _) = primMaterial p
   
type Integrator = Scene -> Ray -> Rand WeightedSpectrum
   
evalLight :: Scene -> Point -> Normal -> Light -> Vector -> Bsdf -> Rand2D -> Spectrum
evalLight scene p n light wo bsdf us = (evalSample scene sample wo bsdf p n) where
   sample = lightSample light p n us
   
evalSample :: Scene -> LightSample -> Vector -> Bsdf -> Point -> Normal -> Spectrum
evalSample scene sample wo bsdf _ n
   | isBlack li || isBlack f = black
   | primIntersects scene (testRay sample) = black
   | otherwise = sScale (f * li) $ (absDot wi n) / lPdf
   where
         lPdf = lightSamplePdf sample
         li = de sample
         wi = lightSampleWi sample
         f = evalBsdf bsdf wo wi
   
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
