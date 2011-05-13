
-- | The functions dealing with colours, radiance and light sources
module Graphics.Bling.Light (
   
   -- * Creating Light sources
   Light, mkPointLight, mkDirectional, mkAreaLight, mkSunSky,

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
   = SoftBox ! Int ! Spectrum -- ^ an infinite area light surrounding the whole scene, emitting a constant amount of light from all directions.
   | Directional ! Int !Spectrum !Normal
   | PointLight !Int !Spectrum !Point
   | AreaLight {
      _alId :: Int,
      _alShape :: S.Shape,
      _areaRadiance :: Spectrum,
      _l2w :: Transform, -- ^ the light-to-world transformation
      _w2l :: Transform -- ^ the world-to-light transformation
      }
   | SunSky
      { _ssId :: Int
      , _basis :: LocalCoordinates
      , _ssd :: SunSkyData
      }
      -- ^ the Perez sun/sky model

-- two lights are considered equal if the have the same id
instance Eq Light where
   l1 == l2 = (lightId l1) == (lightId l2) where
      lightId (AreaLight lid _ _ _ _) = lid
      lightId (Directional lid _ _) = lid
      lightId (PointLight lid _ _) = lid
      lightId (SoftBox lid _) = lid
      lightId (SunSky lid _ _) = lid
      
-- | creates a directional light source
mkDirectional :: Spectrum -> Normal -> Int -> Light
mkDirectional s n lid = Directional lid s (normalize n)

-- | creates a point light source
mkPointLight
   :: Spectrum -- ^ intensity
   -> Point -- ^ position
   -> Int -- ^ light id
   -> Light
mkPointLight r p lid = PointLight lid r p

-- | creates an area @Light@ sources for a gives shape and spectrum
mkAreaLight
   :: S.Shape -- ^ the @Shape@ to create the area light for
   -> Spectrum -- ^ the emission @Spectrum@
   -> Transform -- ^ the @Transform@ which places the @Light@ in the world
   -> Int -- ^ the light id
   -> Light -- ^ the resulting @Light@
mkAreaLight s r t lid = AreaLight lid s r t (inverse t)

-- | creates the Perez sun/sky model
mkSunSky
   :: Vector -- ^ the up vector
   -> Vector -- ^ the east vector
   -> Vector -- ^ the sun direction in world coordinates
   -> Flt -- ^ the sky's turbidity
   -> Int -- ^ the light id
   -> Light
mkSunSky up east sdw turb lid = SunSky lid basis ssd where
   basis = coordinateSystem' (normalize up) (normalize east)
   ssd = initSunSky basis sdw turb

-- | the emission from the surface of an area light source
lEmit :: Light -> Point -> Normal -> Vector -> Spectrum
lEmit (AreaLight _ _ r _ t) _ n' wo'
   | n `dot` wo > 0 = r
   | otherwise = black
   where
      n = transNormal t n'
      wo = transVector t wo'
      
-- all others return black because they are no area light sources
lEmit _ _ _ _ = black

le :: Light -> Ray -> Spectrum
-- area lights must be sampled by intersecting the shape directly and asking
-- that intersection for le
le (AreaLight _ _ _ _ _) _ = black
le (Directional _ _ _) _ = black
le (PointLight _ _ _) _ = black
le (SoftBox _ r) _ = r
le (SunSky _ basis ssd) r = skySpectrum ssd d where
   d = normalize $ worldToLocal basis (rayDir r)
   
-- | samples one light source
sample
   :: Light -- ^ the light to sample
   -> Point -- ^ the point in world space from where the light is viewed
   -> Normal -- ^ the surface normal in world space from where the light is viewed
   -> Rand2D -- ^ the random value for sampling the light
   -> LightSample -- ^ the computed @LightSample@
sample (SoftBox _ r) p n us = lightSampleSB r p n us
sample (Directional _ r d) p n _ = lightSampleD r d p n
sample (PointLight _ r pos) p _ _ = LightSample r' wi ray 1 True where
   r' = sScale r (1 / (sqLen $ pos - p))
   wi = normalize $ pos - p
   ray = segmentRay p pos
sample (AreaLight _ s r l2w w2l) p _ us = LightSample r' wi' ray pd False where
   r' = if ns `dot` (-wi) > 0 then r else black
   p' = transPoint w2l p -- point to be lit in local space
   (ps, ns) = S.sample s p' us -- point in local space
   wi' = transVector l2w wi -- incident vector in world space
   wi = normalize (ps - p') -- incident vector in local space
   pd = S.pdf s p' wi -- pdf (computed in local space)
   ray = transRay l2w (segmentRay ps p') -- vis. test ray (in world space)

sample (SunSky _ basis ssd) p _n us = LightSample r dw ray pd False where
   dl = cosineSampleHemisphere us
   dw = localToWorld basis dl
   pd = invPi * (dw .! dimZ)
   r = skySpectrum ssd dl
   ray = Ray p dw epsilon infinity
      
pdf :: Light -- ^ the light to compute the pdf for
    -> Point -- ^ the point from which the light is viewed
    -> Vector -- ^ the wi vector
    -> Float -- ^ the computed pdf value
pdf (SoftBox _ _) _ _ = undefined
pdf (Directional _ _ _) _ _ = 0 -- zero chance to find the direction by sampling
pdf (AreaLight _ ss _ _ t) p wi = S.pdf ss (transPoint t p) (transVector t wi)
pdf (PointLight _ _ _) _ _ = 0
pdf (SunSky _ _ _) _ _ = 2 * pi

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

--
-- Perez physically based Sun / Sky model
--

type Perez = (Flt, Flt, Flt, Flt, Flt)

data SunSkyData = SSD
   { sunDir :: !Vector
   , sunTheta :: Flt
   , perezx :: !Perez
   , perezy :: !Perez
   , perezY :: !Perez
   , zenithx :: !Flt
   , zenithy :: !Flt
   , zenithY :: !Flt
   }

initSunSky :: LocalCoordinates -> Vector -> Flt -> SunSkyData
initSunSky basis sdw t = SSD sd st px py pY zx zy zY where
   sd = normalize $ worldToLocal basis sdw
   st = acos $ clamp (sd .! dimZ) (-1) 1
   (st2, st3, t2) = (st * st, st * st * st, t * t)
   chi = (4 / 9 - t / 120) * (pi - 2 * st)
   
   pY = ( 0.17872 * t - 1.46303, -0.35540 * t + 0.42749, -0.02266 * t + 5.32505,
          0.12064 * t - 2.57705, -0.06696 * t + 0.37027)
   px = (-0.01925 * t - 0.25922, -0.06651 * t + 0.00081, -0.00041 * t + 0.21247,
         -0.06409 * t - 0.89887, -0.00325 * t + 0.04517)
   py = (-0.01669 * t - 0.26078, -0.09495 * t + 0.00921, -0.00792 * t + 0.21023,
         -0.04405 * t - 1.65369, -0.01092 * t + 0.05291)

   zY = ((4.04530 * t - 4.97100) * tan chi  - 0.2155 * t + 2.4192) * 1000
   zx = ( 0.00165 * st3 - 0.00374 * st2 + 0.00208 * st          ) * t2 +
        (-0.02902 * st3 + 0.06377 * st2 - 0.03202 * st + 0.00394) * t  +
        ( 0.11693 * st3 - 0.21196 * st2 + 0.06052 * st + 0.25885)
   zy = ( 0.00275 * st3 - 0.00610 * st2 + 0.00316 * st          ) * t2 +
        (-0.04212 * st3 + 0.08970 * st2 - 0.04153 * st + 0.00515) * t  +
        ( 0.15346 * st3 - 0.26756 * st2 + 0.06669 * st + 0.26688)

skySpectrum :: SunSkyData -> Vector -> Spectrum
skySpectrum ssd dir@(Vector _ _ dz)
   | dz < 0.001 = black
   | otherwise = fromXYZ (x', y', z')
   where
      (cx, cy, cz) = spdToXYZ $ fromCIExy x y
      (x', z') = (cx * y' / cy, cz * y' / cy)
      theta = acos dz
      gamma = acos $ clamp (dir `dot` (sunDir ssd)) (-1) (1)
      x = perez (perezx ssd) (sunTheta ssd) theta gamma (zenithx ssd)
      y = perez (perezy ssd) (sunTheta ssd) theta gamma (zenithy ssd)
      y' = perez (perezY ssd) (sunTheta ssd) theta gamma (zenithY ssd) * 1e-4
      
perez :: Perez -> Flt -> Flt -> Flt -> Flt -> Flt
perez (p0, p1, p2, p3, p4) sunT t g lvz = lvz * num / den where
   num = (1 + p0 * exp (p1 / cos t)) * (1 + p2 * exp (p3 * g)) + p4 * csg * csg
   den = (1 + p0 * exp p1) * (1 + p2 * exp (p3 * sunT)) + p4 * cst * cst
   csg = cos g
   cst = cos sunT