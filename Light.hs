
-- | The functions dealing with colours, radiance and light sources
module Light (
   mkAreaLight,
   Light(..), LightSample(..), lightSample, lightEmittance, lightLe, lightPdf) where

import Color
import Geometry
import Math
import Random

data LightSample = LightSample {
   de :: Spectrum, -- ^ differential irradiance
   lightSampleWi :: Vector, -- ^ incident direction
   testRay :: Ray, -- ^ for visibility test
   lightSamplePdf :: Float,
   lightSampleDelta :: Bool -- ^ does that light employ a delta-distributuion?
   }

data Light =
   SoftBox Spectrum | -- ^ an infinite area light surrounding the whole scene, emitting a constant amount of light from all directions.
   Directional Spectrum Normal | -- ^ 
   AreaLight Spectrum Float (Point -> Rand2D -> (Point, Normal)) (Point -> Float) |
   ProbeLight ((Float, Float) -> Spectrum) (Normal -> Rand2D -> (Float, (Float, Float)))
   
mkProbeLight :: (LightProbe p) => p -> Light
mkProbeLight p = ProbeLight (lightProbeEval p) (lightProbeSample p)
   
mkAreaLight :: (Bound b) => Spectrum -> b -> Light
mkAreaLight r bound = AreaLight r (boundArea bound) (boundSample bound) (boundPdf bound)

lightLe :: Light -> Point -> Normal -> Normal -> Spectrum
lightLe (AreaLight r _ _ _) _ n wo
   | dot n wo > 0 = r
   | otherwise = black
lightLe _ _ _ _ = black

lightSample :: Light -> Point -> Normal -> Rand2D -> LightSample
lightSample (ProbeLight eval sample) p n u =
   let (pdf, (s, t)) = sample n u
       de = eval (s, t)
       theta = t * pi
       phi = s * 2 * pi
       wi = sphericalDirection (sin theta) (cos theta) phi
       r = Ray p wi epsilon infinity
   in LightSample de wi r pdf False
                                           
lightSample (SoftBox r) p n us = lightSampleSB r p n us
lightSample (Directional r d) p n _ = lightSampleD r d p n
lightSample (AreaLight r _ sample pdf) p n us = LightSample (sScale r (absDot ns n)) wi (segmentRay p ps) (pdf p) False where
   (ps, ns) = sample p us
   wi = normalize $ ps `sub` p

lightEmittance :: Light -> Ray -> Spectrum
lightEmittance (SoftBox r) _ = r
lightEmittance (Directional _ _) _ = black
lightEmittance (AreaLight _ _ _ _) _ = black -- ^ must be sampled by intersecting the shape directly and asking that intersection for le

lightPdf :: Light -> Point -> Normal -> Vector -> Float
lightPdf (SoftBox _) _ n wi = absDot n wi
lightPdf (Directional _ _) _ _ _ = infinity -- zero chance to find the direction by sampling
lightPdf (AreaLight _ _ _ pdf) p _ _ = pdf p

lightSampleSB :: Spectrum -> Point -> Normal -> Rand2D -> LightSample
lightSampleSB r pos n us = LightSample r (toWorld lDir) (ray $ toWorld lDir) (pdf lDir) False
   where
      lDir = cosineSampleHemisphere us
      ray = (\dir -> Ray pos dir epsilon infinity)
      pdf (_, _, z) = invPi * z
      toWorld v = localToWorld (coordinateSystem n) v

lightSampleD :: Spectrum -> Normal -> Point -> Normal -> LightSample
lightSampleD r d pos n = LightSample y d ray 1.0 True where
   y = sScale r (absDot n d)
   ray = (Ray pos d epsilon infinity)

class LightProbe a where
   lightProbeEval :: a -> (Float, Float) -> Spectrum
   lightProbeSample :: a -> Normal -> Rand2D -> (Float, (Float, Float))
   