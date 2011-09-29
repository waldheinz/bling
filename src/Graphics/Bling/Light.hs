
-- | The functions dealing with colours, radiance and light sources
module Graphics.Bling.Light (
   
   -- * Creating Light sources
   Light, mkPointLight, mkDirectional, mkAreaLight, mkInfiniteAreaLight,
   mkSunSkyLight,

   -- * Working with light sources
   LightSample(..), sample, sample', le, lEmit, pdf, power
   ) where

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Random
import qualified Graphics.Bling.Shape as S
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture
import Graphics.Bling.Transform

data LightSample = LightSample {
   de                :: Spectrum,   -- ^ differential irradiance
   lightSampleWi     :: Vector,     -- ^ incident direction
   testRay           :: Ray,        -- ^ for visibility test
   lightSamplePdf    :: Float,      -- ^ the PDF for this sample
   lightSampleDelta  :: Bool        -- ^ does that light employ a delta-distributuion?
   }

data Light
   = Infinite
      { _infRadiance :: SpectrumMap
      , _infDis      :: Dist2D
      , _infw2l      :: Transform }
   | Directional !Spectrum !Normal
   | PointLight !Spectrum !Point
   | AreaLight {
      _alId :: !Int,
      _alShape :: !S.Shape,
      _areaRadiance :: !Spectrum,
      _l2w :: !Transform, -- ^ the light-to-world transformation
      _w2l :: !Transform -- ^ the world-to-light transformation
      }
   | Sky
      { _basis :: LocalCoordinates
      , _ssd :: SkyData
      }
      -- ^ the Perez sun/sky model
   | Sun Vector Spectrum
   
-- two lights are considered equal if they have the same id
instance Eq Light where
   (AreaLight id0 _ _ _ _) == (AreaLight id1 _ _ _ _) = id0 == id1
   _ == _ = False
      
-- | creates a directional light source
mkDirectional :: Spectrum -> Normal -> Light
mkDirectional s n = Directional s (normalize n)

-- | creates a point light source
mkPointLight
   :: Spectrum -- ^ intensity
   -> Point -- ^ position
   -> Light
mkPointLight r p = PointLight r p

-- | creates an area @Light@ sources for a gives shape and spectrum
mkAreaLight
   :: S.Shape -- ^ the @Shape@ to create the area light for
   -> Spectrum -- ^ the emission @Spectrum@
   -> Transform -- ^ the @Transform@ which places the @Light@ in the world
   -> Int -- ^ the light id
   -> Light -- ^ the resulting @Light@
mkAreaLight s r t lid = AreaLight lid s r t (inverse t)

mkInfiniteAreaLight
   :: SpectrumMap
   -> Transform
   -> Light
mkInfiniteAreaLight rmap t = Infinite rmap dist t where
   dist = mkDist2D (texSize rmap) eval
   (sx, sy) = (\(ix, iy) -> (fromIntegral ix, fromIntegral iy)) $ texSize rmap
   eval (x, y) = sY $ texMapEval rmap p where
      p = Cartesian (fromIntegral x / sx, fromIntegral y / sy)
      
-- | creates the Perez sun/sky model
mkSunSkyLight
   :: Vector -- ^ the up vector
   -> Vector -- ^ the east vector
   -> Vector -- ^ the sun direction in world coordinates
   -> Flt -- ^ the sky's turbidity
   -> [Light]
mkSunSkyLight up east sdw turb = [sky, sun] where
   sky = Sky basis ssd
   sun = Sun (normalize sdw) $ sunSpectrum ssd turb
   basis = coordinateSystem' (normalize up) (normalize east)
   ssd = initSky basis sdw turb

-- | the emission from the surface of an area light source
lEmit
   :: Light -- ^ the light to get the radiance from
   -> Point -- ^ the point on the area light in world space
   -> Normal -- ^ the area light's normal at the intersection point
   -> Vector -- ^ the incident direction where the light is cast
   -> Spectrum
lEmit (AreaLight _ _ r _ _) _ n wo
   | n `dot` wo > 0 = r
   | otherwise = black
   
-- all others return black because they are no area light sources
lEmit _ _ _ _ = black

le :: Light -> Ray -> Spectrum
-- area lights must be sampled by intersecting the shape directly and asking
-- that intersection for le
le (AreaLight _ _ _ _ _) _ = black
le (Directional _ _) _ = black
le (PointLight _ _) _ = black
le (Infinite rmap _ w2l) ray = texMapEval rmap $ sphToCart sphDir where
   sphDir = dirToSph wh
   wh = normalize $ transVector w2l $ rayDir ray
le (Sky basis ssd) r = skySpectrum ssd d where
   d = normalize $ worldToLocal basis (rayDir r)
le (Sun sd r) ray
   | (sd `dot` rd) > sunThetaMax = r
   | otherwise = black
   where
      rd = (normalize $ rayDir ray)
      
sunThetaMax :: Flt
sunThetaMax = sqrt $ max 0 (1 - sint2) where
   sint2 = radius / meanDistance
   radius = 6.955e5 -- km
   meanDistance = 1.496e8 -- 149,60 million km

-- | computes the total power of a light source
power
   :: Light -- ^ the light to get the power for
   -> AABB -- ^ the bounds of the area illuminated by this light
   -> Spectrum -- ^ the light's power in [Watt]

power (AreaLight _ s r _ _) _ = sScale r ((S.area s) * pi)
power (Directional r _) b = sScale r $ pi * radius * radius where
   radius = snd $ boundingSphere b
power (PointLight r _) _ = sScale r $ 4 * pi
power (Infinite r _ _) b = sScale (texMapAvg r) $ pi * wr * wr where
   wr = snd $ boundingSphere b

-- | samples one light source
sample
   :: Light -- ^ the light to sample
   -> Point -- ^ the point in world space from where the light is viewed
   -> Normal -- ^ the surface normal in world space from where the light is viewed
   -> Rand2D -- ^ the random value for sampling the light
   -> LightSample -- ^ the computed @LightSample@

sample (Infinite r dist w2l) p _ us
   | mapPdf == 0 = LightSample black (mkV (0,1,0)) (error "empty light sample") 0 False
   | otherwise = LightSample ls wi ray pd False
   where
      (uv, mapPdf) = sampleContinuous2D dist us
      ls = texMapEval r uv
      sphDir = cartToSph uv
      wi = transVector (inverse w2l) $ sphToDir $ sphDir
      ray = Ray p wi epsilon infinity
      pd = mapPdf / (2 * pi * pi * sphSinTheta sphDir)
      

sample (Directional r d) p n _ = lightSampleD r d p n
sample (PointLight r pos) p _ _ = LightSample r' wi ray 1 True where
   r' = sScale r (1 / sqLen (pos - p))
   wi = normalize $ pos - p
   ray = segmentRay p pos
   
sample (AreaLight _ s r l2w w2l) p _ us = LightSample r' wi' ray pd False where
   r' = if ns `dot` wi < 0 then r else black
   p' = transPoint w2l p -- point to be lit in local space
   (ps, ns) = S.sample s p' us -- point in local space
   wi' = transVector l2w wi -- incident vector in world space
   wi = normalize (ps - p') -- incident vector in local space
   pd = S.pdf s p' wi -- pdf (computed in local space)
   ray = transRay l2w (segmentRay ps p') -- vis. test ray (in world space)
{- 
sample (Sky _ basis ssd) p n us = LightSample r dw ray pd False where
   v = cosineSampleHemisphere us
   c2 = coordinateSystem $ normalize n
   dw = localToWorld c2 v
   pd = invPi * (v .! dimZ) * 2
   r = skySpectrum ssd $ normalize $ worldToLocal basis dw
   ray = Ray p dw epsilon infinity
 -}

sample (Sky basis ssd) p _ us = LightSample r dw ray pd False where
   dw = uniformSampleSphere us
   pd = 1 / (4 * pi)
   r = skySpectrum ssd $ normalize $ worldToLocal basis dw
   ray = Ray p dw epsilon infinity

sample (Sun dir r) p n us = LightSample r' d ray pd False where
   r' = sScale r (absDot n d)
   d = uniformSampleCone (coordinateSystem dir) sunThetaMax us
   ray = Ray p d epsilon infinity
   pd = uniformConePdf sunThetaMax

-- | samples an outgoing @Ray@ from a light source
sample'
   :: Light -- ^ the light to sample the ray from
   -> AABB -- ^ the bounds of the region to be lit
   -> Rand2D -- ^ for sampling the position on the light
   -> Rand2D -- ^ for sampling the outgoing direction
   -> (Spectrum, Ray, Normal, Flt)
      -- ^ (radiance, outgoing ray, normal at light source, PDF)

sample' (AreaLight _ s r _ _) _ uo ud = (r, ray, ns, pd) where
   (org, ns) = S.sample' s uo
   pd = invTwoPi * S.pdf' s org
   dir' = uniformSampleSphere ud
   dir = if ns `dot` dir' < 0 then -dir' else dir'
   ray = Ray org dir 1e-2 infinity

pdf :: Light -- ^ the light to compute the pdf for
    -> Point -- ^ the point from which the light is viewed
    -> Vector -- ^ the wi vector
    -> Float -- ^ the computed pdf value
    
pdf (Infinite _ dist w2l) _ wiW
   | sint == 0 = 0
   | otherwise = pdfDist2D dist (sphToCart sph) / (2 * pi * pi * sint)
   where
      sint = sphSinTheta sph
      sph = dirToSph $ transVector w2l wiW

pdf (Directional _ _) _ _ = 0 -- zero chance to find the direction by sampling
pdf (AreaLight _ ss _ _ t) p wi = S.pdf ss (transPoint t p) (transVector t wi)
pdf (PointLight _ _) _ _ = 0
pdf (Sky _ _) _ _ = 1 / (4 * pi) -- invTwoPi
pdf (Sun _ _) _ _ = uniformConePdf sunThetaMax

lightSampleD :: Spectrum -> Normal -> Point -> Normal -> LightSample
lightSampleD r d pos n = LightSample y d ray 1.0 True where
   y = sScale r (absDot n d)
   ray = Ray pos d epsilon infinity

--
-- Perez physically based Sun / Sky model
--

type Perez = (Flt, Flt, Flt, Flt, Flt)

data SkyData = SD
   { sunDir :: !Vector
   , sunTheta :: Flt
   , perezx :: !Perez
   , perezy :: !Perez
   , perezY :: !Perez
   , zenithx :: !Flt
   , zenithy :: !Flt
   , zenithY :: !Flt
   }

initSky :: LocalCoordinates -> Vector -> Flt -> SkyData
initSky basis sdw t = SD sd st px py pY zx zy zY where
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

skySpectrum :: SkyData -> Vector -> Spectrum
skySpectrum ssd dir@(Vector _ _ dz)
   | dz < 0.001 = black
   | otherwise = fromXYZ (x', y', z')
   where
      (cx, cy, cz) = chromaticityToXYZ x y
      (x', z') = (cx * y' / cy, cz * y' / cy)
      theta = acos dz
      gamma = acos $ clamp (dir `dot` sunDir ssd) (-1) 1
      x = perez (perezx ssd) (sunTheta ssd) theta gamma (zenithx ssd)
      y = perez (perezy ssd) (sunTheta ssd) theta gamma (zenithy ssd)
      y' = perez (perezY ssd) (sunTheta ssd) theta gamma (zenithY ssd) * 1e-4
      
perez :: Perez -> Flt -> Flt -> Flt -> Flt -> Flt
perez (p0, p1, p2, p3, p4) sunT t g lvz = lvz * num / den where
   num = (1 + p0 * exp (p1 / cos t)) * (1 + p2 * exp (p3 * g)) + p4 * csg * csg
   den = (1 + p0 * exp p1) * (1 + p2 * exp (p3 * sunT)) + p4 * cst * cst
   csg = cos g
   cst = cos sunT
   
sunSpectrum :: SkyData -> Flt -> Spectrum
sunSpectrum ssd turb
   | (sunDir ssd) .! dimZ < 0 = black -- below horizon
   | otherwise = fromSpd $ mkSpdFunc sf
   where
      t = sunTheta ssd
      sf l = (evalSpd solCurve l) * tR * tA * tO * tG * tWA where
         -- relative optical mass
         m = 1 / (cos t + 0.000940 * ((1.6386 - t) **  (-1.253)))
         
         -- rayleigh scattering
         tR = exp(-m * 0.008735 * ((l / 1000) ** (-4.08)))
         
         -- aerosol (water + dust) attenuation
         alpha = 1.3
         beta = 0.04608365822050 * turb - 0.04586025928522
         tA = exp(-m * beta * ((l / 1000) ** (-alpha)))
         
         -- attenuation due to ozone absorption
         lozone = 0.35
         tO = exp $ -m * (evalSpd koCurve l) * lozone
         
         -- attenuation due to mixed gases absorption
         kg = evalSpd kgCurve l
         tG = exp $ -1.41 * kg * m / ((1.0 + 118.93 * kg * m) ** 0.45)
         
         -- attenuation due to water vapor absorption
         kwa = evalSpd kwaCurve l
         w = 2
         tWA = exp $ -0.2385 * kwa * w * m / ((1 + 20.07 * kwa * w * m) ** 0.45)
         
solCurve :: Spd
solCurve = mkSpd' amps 380 750 where
   amps = [ 165.5, 162.3, 211.2, 258.8, 258.2, 242.3, 267.6, 296.6, 305.4,
            300.6, 306.6, 288.3, 287.1, 278.2, 271.0, 272.3, 263.6, 255.0,
            250.6, 253.1, 253.5, 251.3, 246.3, 241.7, 236.8, 232.1, 228.2,
            223.4, 219.7, 215.3, 211.0, 207.3, 202.4, 198.7, 194.3, 190.7,
            186.3, 182.6 ]
      
koCurve :: Spd
koCurve = mkSpd $ zip ls as where
   ls = [ 300, 305, 310, 315, 320, 325, 330, 335, 340, 345, 350, 355, 445, 450,
          455, 460, 465, 470, 475, 480, 485, 490, 495, 500, 505, 510, 515, 520,
          525, 530, 535, 540, 545, 550, 555, 560, 565, 570, 575, 580, 585, 590,
          595, 600, 605, 610, 620, 630, 640, 650, 660, 670, 680, 690, 700, 710,
          720, 730, 740, 750, 760, 770, 780, 790 ]
   as = [ 10.0, 4.8, 2.7, 1.35, 0.8, 0.380, 0.160, 0.075, 0.04, 0.019, 0.007, 0,
          0.003, 0.003, 0.004, 0.006, 0.008, 0.009, 0.012, 0.014, 0.017, 0.021,
          0.025, 0.03, 0.035, 0.04, 0.045, 0.048, 0.057, 0.063, 0.07, 0.075,
          0.08, 0.085, 0.095, 0.103, 0.110, 0.12, 0.122, 0.12, 0.118, 0.115,
          0.12, 0.125, 0.130, 0.12, 0.105, 0.09, 0.079, 0.067, 0.057, 0.048,
          0.036, 0.028, 0.023, 0.018, 0.014, 0.011, 0.010, 0.009, 0.007,
          0.004, 0, 0 ]

kgCurve :: Spd
kgCurve = mkSpd $ zip [759, 760, 770, 771] [0, 3.0, 0.210, 0]
   
kwaCurve :: Spd
kwaCurve = mkSpd $ zip ls as where
   ls = [ 689, 690, 700, 710, 720, 730, 740, 750, 760, 770, 780, 790, 800 ]
   as = [ 0, 0.160e-1, 0.240e-1, 0.125e-1, 0.100e+1, 0.870, 0.610e-1,
          0.100e-2, 0.100e-4, 0.100e-4, 0.600e-3, 0.175e-1, 0.360e-1 ]
   
