
-- | The functions dealing with colours, radiance and light sources
module Graphics.Bling.Light (
   
   -- * Creating Light sources
   Light, mkDirectional, mkAreaLight,

   -- * Working with light sources
   LightSample(..), sample, le, lEmit, pdf
   ) where
   
import Graphics.Bling.Math
import Graphics.Bling.Random
import qualified Graphics.Bling.Shape as S
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

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
      _alShape :: S.Shape,
      _areaRadiance :: Spectrum,
      _l2w :: Transform, -- ^ the light-to-world transformation
      _w2l :: Transform -- ^ the world-to-light transformation
      }

-- | creates a new directional light source
mkDirectional :: Spectrum -> Normal -> Light
mkDirectional s n = Directional s (normalize n)

-- | creates a new area light sources
mkAreaLight :: S.Shape -> Spectrum -> Transform -> Light
mkAreaLight s r t = AreaLight s r t (inverse t)

-- | the emission from the surface of an area light source
lEmit :: Light -> Point -> Normal -> Vector -> Spectrum
lEmit (AreaLight _ r _ t) _ n' wo'
   | n `dot` wo > 0 = r
   | otherwise = black
   where
      n = transNormal t n'
      wo = transVector t wo'
      
-- all others return black because they are no area light sources
lEmit _ _ _ _ = black

le :: Light -> Ray -> Spectrum
le (SoftBox r) _ = r
le (Directional _ _) _ = black
-- area lights must be sampled by intersecting the shape directly and asking
-- that intersection for le
le (AreaLight _ _ _ _) _ = black

-- | samples one light source
sample
   :: Light -- ^ the light to sample
   -> Point -- ^ the point in world space from where the light is viewed
   -> Normal -- ^ the surface normal in world space from where the light is viewed
   -> Rand2D -- ^ the random value for sampling the light
   -> LightSample -- ^ the computed @LightSample@
sample (SoftBox r) p n us = lightSampleSB r p n us
sample (Directional r d) p n _ = lightSampleD r d p n
sample al@(AreaLight s _ t t') p _ us = LightSample ls wi' ray pd False where
   p' = transPoint t' p -- point in local space
   (ps, ns) = S.sample s p' us -- point and normal in local space
   wi' = transVector t wi -- incident vector in world space
   wi = normalize (ps - p') -- incident vector in local space
   ls = lEmit al ps ns (-wi) -- emitted light (computed in local space)
   pd = pdf al p' wi -- pdf (computed in local space)
   ray = transRay t (segmentRay p' ps) -- vis. test ray (in world space)
   
pdf :: Light -- ^ the light to compute the pdf for
    -> Point -- ^ the point from which the light is viewed
    -> Vector -- ^ the wi vector
    -> Float -- ^ the computed pdf value
pdf (SoftBox _) _ _ = undefined
pdf (Directional _ _) _ _ = infinity -- zero chance to find the direction by sampling
pdf (AreaLight ss _ _ t) p wi = S.pdf ss (transPoint t p) (transVector t wi)

lightSampleSB :: Spectrum -> Point -> Normal -> Rand2D -> LightSample
lightSampleSB r pos n us = LightSample r (toWorld lDir) (ray $ toWorld lDir) (p lDir) False
   where
      lDir = cosineSampleHemisphere us
      ray dir = Ray pos dir epsilon infinity
      p (Vector _ _ z) = invPi * z
      toWorld = localToWorld (coordinateSystem n)

lightSampleD :: Spectrum -> Normal -> Point -> Normal -> LightSample
lightSampleD r d pos n = LightSample y d ray 1.0 True where
   y = sScale r (absDot n d)
   ray = Ray pos d epsilon infinity
