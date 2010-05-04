
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
   Directional Spectrum Normal |
   AreaLight Spectrum AnyBound
   
lightLe :: Light -> Point -> Normal -> Normal -> Spectrum
lightLe (AreaLight r _) _ n wo
   | dot n wo > 0 = r
   | otherwise = black
lightLe _ _ _ _ = black

lightSample :: Light -> Point -> Normal -> Rand2D -> LightSample  
lightSample (SoftBox r) p n us = lightSampleSB r p n us
lightSample (Directional r d) p n _ = lightSampleD r d p n
lightSample (AreaLight r b) p n us = sampleAreaLight b r p n us 

lightEmittance :: Light -> Ray -> Spectrum
lightEmittance (SoftBox r) _ = r
lightEmittance (Directional _ _) _ = black
lightEmittance (AreaLight _ _) _ = black -- ^ must be sampled by intersecting the shape directly and asking that intersection for le

lightPdf :: Light -> Point -> Normal -> Vector -> Float
lightPdf (SoftBox _) _ n wi = absDot n wi
lightPdf (Directional _ _) _ _ _ = infinity -- zero chance to find the direction by sampling
lightPdf (AreaLight _ b) p _ _ = boundPdf b p

sampleAreaLight :: (Bound a) => a -> Spectrum -> Point -> Normal -> Rand2D -> LightSample
sampleAreaLight shape lemit p n = do
   (ps, ns) <- boundSample shape p
   wi <- return $ normalize $ ps `sub` p
   return $! LightSample (sScale lemit (absDot ns n)) wi (segmentRay p ps) (boundPdf shape p)

lightSampleSB :: Spectrum -> Point -> Normal -> Rand2D -> LightSample
lightSampleSB r pos n us = sample lDir
   where
      lDir = cosineSampleHemisphere us
      sample = (\ds -> LightSample r (toWorld ds) (ray $ toWorld ds) (pdf ds))
      ray = (\dir -> Ray pos dir epsilon infinity)
      
      pdf :: Vector -> Float
      pdf (_, _, z) = invPi * z
      
      toWorld :: Vector -> Vector
      toWorld v = localToWorld (coordinateSystem n) v

lightSampleD :: Spectrum -> Normal -> Point -> Normal -> LightSample
lightSampleD r d pos n = LightSample y d ray 1.0 where
   y = sScale r (absDot n d)
   ray = (Ray pos d epsilon infinity)

    