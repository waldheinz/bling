
-- | The functions dealing with colours, radiance and light sources
module Graphics.Bling.Light (
   
   -- * Creating Light sources
   Light, mkPointLight, mkDirectional, mkAreaLight, mkInfiniteAreaLight,

   -- * Working with light sources
   LightSample(..), sample, sample', le, lEmit, pdf, power
   ) where

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Random
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture
import Graphics.Bling.Transform

data LightSample = LightSample {
   de                :: !Spectrum,                 -- ^ differential irradiance
   lightSampleWi     :: {-# UNPACK #-} !Vector,    -- ^ incident direction
   testRay           :: !Ray,                      -- ^ for visibility test
   lightSamplePdf    :: {-# UNPACK #-} !Float,     -- ^ the PDF for this sample
   lightSampleDelta  :: !Bool                      -- ^ does that light employ a delta-distributuion?
   }

data Light
   = Infinite
      { _infRadiance :: SpectrumMap
      , _infAvg      :: Spectrum
      , _infDis      :: Dist2D
      , _infw2l      :: Transform }
   | Directional !Spectrum !Normal
   | PointLight !Spectrum !Point
   | AreaLight {
      _alId          :: !Int,
      _alShape       :: !Shape,
      _areaRadiance  :: !Spectrum,
      _l2w           :: !Transform, -- ^ light-to-world
      _w2l           :: !Transform  -- ^ world-to-light
      }
      
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
   :: Shape -- ^ the @Shape@ to create the area light for
   -> Spectrum -- ^ the emission @Spectrum@
   -> Transform -- ^ the @Transform@ which places the @Light@ in the world
   -> Int -- ^ the light id
   -> Light -- ^ the resulting @Light@
mkAreaLight s r t lid = AreaLight lid s r t (inverse t)

mkInfiniteAreaLight
   :: SpectrumMap
   -> Transform
   -> Light
mkInfiniteAreaLight rmap t = Infinite rmap avg dist t where
   dist = mkDist2D (texSize rmap) (sY . eval)
   (sx, sy) = (\(ix, iy) -> (fromIntegral ix, fromIntegral iy)) $ texSize rmap
   (mx, my) = texSize rmap
   avg = sScale (sum $ map eval [(x, y) | y <- [0..my], x <- [0..mx]]) (1 / sx * sy)
   eval (x, y) = texMapEval rmap p where
      p = Cartesian (fromIntegral x / sx, fromIntegral y / sy)
      
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
le (Infinite rmap _ _ w2l) ray = texMapEval rmap $ sphToCart sphDir where
   sphDir = dirToSph wh
   wh = normalize $ transVector w2l $ rayDir ray

-- | computes the total power of a light source
power
   :: Light -- ^ the light to get the power for
   -> AABB -- ^ the bounds of the area illuminated by this light
   -> Spectrum -- ^ the light's power in [Watt]

power (AreaLight _ s r _ _) _ = sScale r ((area s) * pi)
power (Directional r _) b = sScale r $ pi * radius * radius where
   radius = snd $ boundingSphere b
power (PointLight r _) _ = sScale r $ 4 * pi
power (Infinite _ r _ _) b = sScale r $ pi * wr * wr where
   wr = snd $ boundingSphere b

-- | samples one light source
sample
   :: Light    -- ^ the light to sample
   -> Point    -- ^ the point in world space from where the light is viewed
   -> Float      -- ^ epsilon for ray starting point
   -> Normal   -- ^ the surface normal in world space from where the light is viewed
   -> Rand2D   -- ^ the random value for sampling the light
   -> LightSample -- ^ the computed @LightSample@

sample (Infinite r _ dist w2l) p eps _ us
   | mapPdf == 0 = LightSample black (mkV (0,1,0)) (Ray (mkV' 0 0 0) (mkV' 0 1 0) 0 1) 0 False
   | sint == 0 = LightSample black (mkV (0,1,0)) (Ray (mkV' 0 0 0) (mkV' 0 1 0) 0 1) 0 False
   | otherwise = LightSample ls wi ray pd False
   where
      (uv, mapPdf) = sampleContinuous2D dist us
      ls = texMapEval r uv
      sphDir = cartToSph uv
      wi = transVector (inverse w2l) $ sphToDir $ sphDir
      ray = Ray p wi eps infinity
      sint = sphSinTheta sphDir
      pd = mapPdf / (2 * pi * pi * sint)
      
sample (Directional r d) p e n _ = LightSample y d ray 1 True where
   y = sScale r (absDot n d)
   ray = Ray p d e infinity

sample (PointLight r pos) p e _ _ = LightSample r' wi ray 1 True where
   r' = sScale r (1 / sqLen (pos - p))
   wi = normalize $ pos - p
   ray = Ray p (pos - p) e infinity
   
sample (AreaLight _ s r l2w w2l) pW e _ us = LightSample r' wiW rayW pd False where
   r' = if ns `dot` wi < 0 then r else black
   p = transPoint w2l pW -- point to be lit in local space
   (ps, ns) = sampleShape s p us -- point in local space
   wiW = transVector l2w wi -- incident vector in world space
   wi = normalize $ ps - p -- incident vector in local space
   pd = shapePdf s p wi -- pdf (computed in local space)
   rayW = transRay l2w ray -- vis. test ray (in world space)
   ray = Ray p wi e (len (ps - p) - e) -- vis. test ray (in local space)

emptySample' :: (Spectrum, Ray, Normal, Float)
emptySample' = (black, Ray (mkV (0, 0, 0)) (mkV (0, 1, 0)) 0 0, mkV (0, 1, 0), 0)

-- | samples an outgoing @Ray@ from a light source
sample'
   :: Light -- ^ the light to sample the ray from
   -> AABB -- ^ the bounds of the region to be lit
   -> Rand2D -- ^ for sampling the position on the light
   -> Rand2D -- ^ for sampling the outgoing direction
   -> (Spectrum, Ray, Normal, Float)
      -- ^ (radiance, outgoing ray, normal at light source, PDF)

sample' (AreaLight _ s r t _) _ uo ud = (r, ray, ns, pd) where
   (org', ns') = sampleShape' s uo
   (org, ns) = (transPoint t org', transNormal t ns')
   pd = invTwoPi * shapePdf' s org
   wi = let d = uniformSampleSphere ud in if ns `dot` d < 0 then -d else d
   ray = Ray org wi 1e-3 infinity

sample' (Directional r n) bounds uo _ = (r, ray, ns, pd) where
   (wc, wr) = boundingSphere bounds
   (v1, v2) = coordinateSystem'' n
   (d1, d2) = concentricSampleDisk uo
   pdisk = wc + wr *# (d1 *# v1 + d2 *# v2)
   ns = (-n)
   ray = Ray (pdisk + wr *# n) ns 0 infinity
   pd = 1 / (pi * wr * wr)
   
sample' (Infinite rmap _ dist w2l) bounds uo ud
   | pdMap == 0 = emptySample'
   | otherwise = (ls, ray, d, pd) where
      -- find sample coordinates
      (uv, pdMap) = sampleContinuous2D dist ud
      ls = texMapEval rmap uv
      sphDir = cartToSph uv
      d = transVector (inverse w2l) $ sphToDir $ sphDir
      -- find Ray
      (worldCenter, worldRad) = boundingSphere bounds
      (LocalCoordinates v1 v2 _) = coordinateSystem (-d)
      (d1, d2) = concentricSampleDisk uo
      pDisk = worldCenter + vpromote worldRad * (vpromote d1 * v1 + vpromote d2 * v2)
      ray = Ray (pDisk + vpromote worldRad * (-d)) d 0 infinity
      -- find PDF
      sint = sphSinTheta sphDir
      pdDir = pdMap / (2 * pi * pi * sint)
      pdArea = 1 / (pi * worldRad * worldRad)
      pd = if sint == 0 then 0 else pdDir * pdArea

sample' (PointLight r p) _ _ ud = (r, ray, d, pd) where
   d = uniformSampleSphere ud
   ray = Ray p d 0 infinity
   pd = uniformSpherePdf
      
pdf :: Light -- ^ the light to compute the pdf for
    -> Point -- ^ the point from which the light is viewed
    -> Vector -- ^ the wi vector
    -> Float -- ^ the computed pdf value
    
pdf (Infinite _ _ dist w2l) _ wiW
   | sint == 0 = 0
   | otherwise = pdfDist2D dist (sphToCart sph) / (2 * pi * pi * sint)
   where
      sint = sphSinTheta sph
      sph = dirToSph $ transVector w2l wiW

pdf (Directional _ _) _ _ = 0 -- zero chance to find the direction by sampling
pdf (AreaLight _ ss _ _ t) p wi = shapePdf ss (transPoint t p) (transVector t wi)
pdf (PointLight _ _) _ _ = 0
