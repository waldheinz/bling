module Scene where

import Control.Monad

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
   
evalLight :: Scene -> Point -> Normal -> Light -> Vector -> Bsdf -> Rand Spectrum
evalLight scene p n light wo bsdf = do
   sample <- lightSample light p n
   return $! (evalSample scene sample wo bsdf p n)
   
evalSample :: Scene -> LightSample -> Vector -> Bsdf -> Point -> Normal -> Spectrum
evalSample scene sample wo bsdf _ n = if (isBlack li || isBlack f)
                then black
                else if (not hidden)
                        then ld
                        else black
   where
         ld = sScale f $ scalMul li $ (absDot wi n) / (lightSamplePdf sample)
         li = de sample
         wi = lightSampleWi sample
         f = evalBsdf bsdf wo wi
         hidden = primIntersects scene (testRay sample)

-- | samples all lights by sampling individual lights and summing up the results
sampleAllLights :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleAllLights scene p n wo bsdf = (foldl (liftM2 add) (return black) spectri) -- sum up contributions
  where
    spectri = map (\l -> evalLight scene p n l wo bsdf) lights
    lights = sceneLights scene

-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleOneLight (Scene _ []) _ _ _ _ = return black -- no light sources -> no light
sampleOneLight scene@(Scene _ (l:[])) p n wo bsdf = evalLight scene p n l wo bsdf -- eval the only light source
sampleOneLight scene@(Scene _ lights) p n wo bsdf = do
  lightNum <-rndR (0, lightCount - 1 :: Int)
  y <- evalLight scene p n (lights !! lightNum) wo bsdf
  return $! scale y
  where
    lightCount = length lights
    scale = (\y -> scalMul y (fromIntegral lightCount))
