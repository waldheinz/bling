
-- | The functions dealing with colours, radiance and light sources
module Light (
   Spectrum, black, white, isBlack, sScale,
   Light(..), Directional(..), SoftBox(..),
   sampleOneLight, sampleAllLights) where

import Control.Monad

import Bsdf
import Geometry
import Math
import Random

data LightSample = LightSample {
   de :: Spectrum, -- ^ differential irradiance
   lightSampleWi :: Vector, -- ^ incident direction
   testRay :: Ray -- ^ for visibility test
   }

class Light a where
   lightSample :: a -> Intersection -> Rand LightSample
   lightEmittance :: a -> Ray -> Spectrum
   lightPdf :: a -> Intersection -> Vector -> Float

-- | An infinite area light; that is a "sphere of light" surrounding the whole scene,
-- emitting a constant amount of light from all directions.
data SoftBox = SoftBox {
   softBoxRadiance :: Spectrum -- ^ the radiance emitted by this light
   }

instance Light SoftBox where
   lightSample (SoftBox r) (Intersection _ pos n) = do
      rndD <- randomOnSphere
 --     dir <- return (sameHemisphere rndD n)
      dir <- return (0,0,0)
      return (LightSample r dir (Ray pos dir epsilon infinity))
      
   lightEmittance (SoftBox r) _ = r
   lightPdf _ i wi = absDot (intNorm i) wi
   
-- | A directional light is a light source where for every point illuminated,
-- the light arrives from the same direction. This like a point light at
-- infinite distance.
data Directional = Directional {
   directionalDir :: Normal, -- ^ the direction this light emits to
   directionalRadiance :: Spectrum -- ^ the spectrum emitted by this light
   }
   
instance Light Directional where
   lightSample dl (Intersection _ pos n) = return (LightSample y lDir ray) where
      y = scalMul (directionalRadiance dl) (abs (dot n lDir))
      ray = (Ray pos (neg lDir) epsilon infinity)
      lDir = directionalDir dl
      
   lightEmittance (Directional _ r) _ = r
   lightPdf _ _ _ = 0
   
evalLight :: (Light l, Intersectable w) => w -> Intersection -> l -> Vector -> Bsdf -> Rand Spectrum
evalLight shape int light wo bsdf = do
   sample <- lightSample light int
   return (evalSample sample shape wo bsdf int)
   
evalSample :: (Intersectable i) => LightSample -> i -> Vector -> Bsdf -> Intersection -> Spectrum
evalSample sample shape wo bsdf int = if (isBlack li || isBlack f)
                then black
                else if (not hidden)
                        then ld
                        else black
   where
         ld = scalMul (sScale f li) (absDot wi (intNorm int))
         li = de sample
         wi = lightSampleWi sample
         f = evalBsdf bsdf wo wi
         hidden = intersects ray shape
         ray = testRay sample

-- | samples all lights by sampling individual lights and summing up the results
sampleAllLights :: (Light l, Intersectable i) => i -> [l] -> Intersection -> Vector -> Bsdf -> Rand Spectrum
sampleAllLights _ [] _ _ _ = return black -- no light source means no light
sampleAllLights shape lights i wo bsdf = (foldl (liftM2 add) (return black) spectri) -- sum up contributions
  where
    spectri = map (\l -> evalLight shape i l wo bsdf) lights

-- | samples one randomly chosen light source
sampleOneLight :: (Light a, Intersectable i) => i -> [a] -> Intersection -> Vector -> Bsdf -> Rand Spectrum
sampleOneLight _ [] _ _ _ = return black -- no light sources -> no light
sampleOneLight shape (light:[]) i wo bsdf = evalLight shape i light wo bsdf -- eval the only light source
sampleOneLight shape lights i wo bsdf = do
  lightNum <-rndR (0, lightCount - 1 :: Int)
  y <- evalLight shape i (lights !! lightNum) wo bsdf
  return $! scale y
  where
    lightCount = length lights
    scale = (\y -> scalMul y (fromIntegral lightCount))
    