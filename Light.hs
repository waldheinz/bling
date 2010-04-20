
-- | The functions dealing with colours, radiance and light sources
module Light (
   Spectrum, black, white, isBlack, sScale,
   Light(..), LightSample(..), lightSample, lightEmittance, lightLe, lightPdf) where

import Color
import Geometry
import Math
import Random

data LightSample = LightSample {
   de :: Spectrum, -- ^ differential irradiance
   lightSampleWi :: Vector, -- ^ incident direction
   testRay :: Ray, -- ^ for visibility test
   lightSamplePdf :: Float
   }

data Light =
   SoftBox Spectrum | -- ^ An infinite area lightsurrounding the whole scene, emitting a constant amount of light from all directions.
   Directional Normal Spectrum |
   AreaLight Spectrum AnyBound
   
lightLe :: Light -> Point -> Normal -> Normal -> Spectrum
lightLe (AreaLight r _) _ n wo
   | dot n wo > 0 = r
   | otherwise = black
lightLe _ _ _ _ = black

lightSample :: Light -> Point -> Normal -> Rand LightSample  
lightSample (SoftBox r) p n = lightSampleSB r p n
lightSample (Directional r d) p n = lightSampleD r d p n
lightSample (AreaLight r b) p n = sampleAreaLight b r p n

lightEmittance :: Light -> Ray -> Spectrum
lightEmittance (SoftBox r) _ = r
lightEmittance (Directional _ _) _ = black
lightEmittance (AreaLight _ _) _ = black -- ^ must be sampled by intersecting the shape directly and asking that intersection for le

lightPdf :: Light -> Point -> Normal -> Vector -> Float
lightPdf (SoftBox _) _ n wi = absDot n wi
lightPdf (Directional _ _) _ _ _ = 0.0 -- zero chance to find the direction by sampling
lightPdf (AreaLight _ b) p _ wi = boundPdf b p wi

sampleAreaLight :: (Bound a) => a -> Spectrum -> Point -> Normal -> Rand LightSample
sampleAreaLight shape lemit p _ = do
   (ps, _) <- boundSample shape p
   wi <- return $ normalize $ ps `sub` p
   return $! LightSample lemit wi (Ray ps (sub p ps) epsilon (1 - epsilon)) (boundPdf shape p wi)

lightSampleSB :: Spectrum -> Point -> Normal -> Rand LightSample
lightSampleSB r pos n = do
   lDir <- cosineSampleHemisphere -- dir in local coordinate system
   return $! sample lDir
   where
      sample = (\ds -> LightSample r (toWorld ds) (ray $ toWorld ds) (pdf ds))
      ray = (\dir -> Ray pos dir epsilon infinity)
      
      pdf :: Vector -> Float
      pdf (_, _, z) = invPi * z
      
      toWorld :: Vector -> Vector
      toWorld v = localToWorld cs v where
         (v1, v2) = coordinateSystem n
         cs = LocalCoordinates v1 v2 n

lightSampleD :: Spectrum -> Normal -> Point -> Normal -> Rand LightSample
lightSampleD r d pos n = return (LightSample y d ray 1.0) where
   y = scalMul r (absDot n d)
   ray = (Ray pos d epsilon infinity)

    