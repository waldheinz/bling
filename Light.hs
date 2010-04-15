
module Light where

import Control.Monad

import Geometry
import Math
import Random

---
--- colours
---
type Spectrum = Vector -- RGB for now

black :: Spectrum
black = (0, 0, 0)

white :: Spectrum
white = (1, 1, 1)

isBlack :: Spectrum -> Bool
isBlack (r, g, b) = r < epsilon && g < epsilon && b < epsilon

---
--- Lights
---

data Directional = Directional {
   dir :: Normal, -- the direction this light emits to
   radiance :: Spectrum }

data LightSample = LightSample {
   de :: Spectrum, -- differential irradiance
   wo :: Vector, -- incident direction
   testRay :: Ray } -- for visibility test

class Light a where
   sampleLight :: a -> Intersection -> Rand LightSample

instance Light Directional where
   sampleLight dl (pos, n, _) = return (LightSample y (dir dl) ray) where
      y = scalMul (radiance dl) (abs (dot n lDir))
      ray = (pos, neg lDir)
      lDir = dir dl

evalLight :: (Light a) => Shape -> Intersection -> a -> Rand Spectrum
evalLight shape int light = do
   y <- liftM de sample
   hidden <- (liftM2 intersects) ray (return shape)
   if (isBlack y)
      then return black
      else if (not hidden)
              then return y
              else return black
              
   where
         sample = sampleLight light int
         ray = ((liftM testRay) sample)


--- samples all lights by sampling individual lights and summing up the results
sampleAllLights :: (Light a) => Shape -> [a] -> Intersection -> Rand Spectrum
sampleAllLights _ [] _ = return black -- no light source means no light
sampleAllLights shape lights i  = (foldl (liftM2 add) (return black) spectri) -- sum up contributions
  where
    spectri = map (evalLight shape i) lights

 -- samples one randomly chosen light source
sampleOneLight :: (Light a) => Shape -> [a] -> Intersection -> Rand Spectrum
sampleOneLight shape lights i = do
  lightNum <-rndR (0, lightCount - 1)
  y <- return (evalLight shape i (lights !! lightNum))
  ((liftM2 scalMul) y (return (fromIntegral lightCount))) -- scale by probability choosing that light
  where
    lightCount = length lights
    
    