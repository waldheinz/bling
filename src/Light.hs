
-- | The functions dealing with colours, radiance and light sources
module Light (
   
   -- * Creating Light sources
   Light, mkDirectional, mkAreaLight,

   -- * Working with light sources
   LightSample(..), Light.sample, le, lEmit, Light.pdf
   ) where
   
import Math
import Random
import Shape as S
import Spectrum

data LightSample = LightSample {
   de :: Spectrum, -- ^ differential irradiance
   lightSampleWi :: Vector, -- ^ incident direction
   testRay :: Ray, -- ^ for visibility test
   lightSamplePdf :: Float,
   lightSampleDelta :: Bool -- ^ does that light employ a delta-distributuion?
   }

data Light
   = SoftBox Spectrum -- ^ an infinite area light surrounding the whole scene, emitting a constant amount of light from all directions.
   | Directional !Spectrum !Normal
   | AreaLight {
      alShape :: Shape,
      areaRadiance :: Spectrum
      }

-- | creates a new directional light source
mkDirectional :: Spectrum -> Normal -> Light
mkDirectional s n = Directional s (normalize n)

-- | creates a new area light sources
mkAreaLight :: Shape -> Spectrum -> Light
mkAreaLight = AreaLight

-- | the emission from the surface of an area light source
lEmit :: Light -> Point -> Normal -> Vector -> Spectrum
lEmit (AreaLight _ r) _ n wo
   | dot n wo > 0 = r
   | otherwise = black
lEmit _ _ _ _ = black
      
le :: Light -> Ray -> Spectrum
le (SoftBox r) _ = r
le (Directional _ _) _ = black
-- area lights must be sampled by intersecting the shape directly and asking
-- that intersection for le
le (AreaLight _ _) r = black

sample :: Light -> Point -> Normal -> Rand2D -> LightSample
sample (SoftBox r) p n us = lightSampleSB r p n us
sample (Directional r d) p n _ = lightSampleD r d p n
sample al@(AreaLight s r) p n us = LightSample ls wi ray pd False where
   (ps, ns) = S.sample s p us
   wi = normalize (ps `sub` p)
   ls = lEmit al ps ns (neg wi)
   pd = Light.pdf al p wi
   ray = segmentRay p ps
   
pdf :: Light -- ^ the light to compute the pdf for
    -> Point -- ^ the point from which the light is viewed
    -> Vector -- ^ the wi vector
    -> Float -- ^ the computed pdf value
pdf (SoftBox _) _ wi = undefined
pdf (Directional _ _) _ _ = infinity -- zero chance to find the direction by sampling
pdf (AreaLight ss _) p wi = S.pdf ss p wi

lightSampleSB :: Spectrum -> Point -> Normal -> Rand2D -> LightSample
lightSampleSB r pos n us = LightSample r (toWorld lDir) (ray $ toWorld lDir) (p lDir) False
   where
      lDir = cosineSampleHemisphere us
      ray dir = Ray pos dir epsilon infinity
      p (MkVector _ _ z) = invPi * z
      toWorld = localToWorld (coordinateSystem n)

lightSampleD :: Spectrum -> Normal -> Point -> Normal -> LightSample
lightSampleD r d pos n = LightSample y d ray 1.0 True where
   y = sScale r (absDot n d)
   ray = Ray pos d epsilon infinity
