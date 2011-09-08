
module Graphics.Bling.Shape (
   Shape,

   -- * Triangles and Meshes
   Vertex(..), triangulate,
   
   -- * Creating shapes
   
   mkSphere, mkCylinder,

   -- * Working with shapes
   
   area, sample, sample', pdf, pdf',
   objectBounds, worldBounds, intersect, intersects
   ) where

import Data.List (foldl')
import Data.Maybe

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Random
import Graphics.Bling.Transform

data Vertex = Vertex {
   vertexPos :: {-# UNPACK #-} !Point
   } deriving (Eq, Show)   

data Shape
   = Sphere
      {-# UNPACK #-} !Flt -- ^ radius
   | Triangle
      Vertex Vertex Vertex
   | Cylinder
      {-# UNPACK #-}!Flt {-# UNPACK #-}!Flt {-# UNPACK #-}!Flt {-# UNPACK #-}!Flt -- ^ radius zmin zmax phimax
   deriving (Eq, Show)
   
-- | creates a sphere
mkSphere :: Flt -> Shape
mkSphere = Sphere

-- | creates a cylinder along the z-axis
mkCylinder
   :: Flt -- ^ the radius
   -> Flt -- ^ the minimum z value
   -> Flt -- ^ the maximum z value
   -> Flt -- ^ the maximum phi angle [in degrees]
   -> Shape
mkCylinder r z0 z1 phimax = Cylinder r zmin zmax pm where
   zmin = min z0 z1
   zmax = max z0 z1
   pm = radians $ clamp phimax 0 360

triangulate :: [[Vertex]] -> [Shape]
triangulate vs = concatMap tr' vs where
   tr' (v1:v2:v3:xs) = Triangle v1 v2 v3 : tr' (v1:v3:xs)
   tr' _ = []

intersect :: Shape -> Ray -> Maybe (Flt, DifferentialGeometry)

intersect (Cylinder r zmin zmax phimax) ray@(Ray ro rd tmin tmax) =
   solveQuadric a b c >>= intersectCylinder >>= \hp -> Just (params hp) where
      a = (vx rd) * (vx rd) + (vy rd) * (vy rd)
      b = 2 * ((vx rd) * (vx ro) + (vy rd) * (vy ro))
      c = (vx ro) * (vx ro) + (vy ro) * (vy ro) - r * r
      
      -- hit point and phi
      intersectCylinder (t0, t1)
         | t0 > tmax = Nothing
         | t1 < tmin = Nothing
         | t > tmax = Nothing
         | vz pHit0 > zmin && vz pHit0 < zmax && phi0 <= phimax =
            Just (pHit0, phi0, t0)
         | t == t1 = Nothing
         | vz pHit1 > zmin && vz pHit1 < zmax && phi1 <= phimax =
            Just (pHit1, phi1, t1)
         | otherwise = Nothing
         where
            t = if t0 > tmin then t0 else t1
            pHit0 = rayAt ray t0
            pHit1 = rayAt ray t1
            phi0 = atan2' (vy pHit0) (vx pHit0)
            phi1 = atan2' (vy pHit1) (vx pHit1)
            
      -- parametric representation
      params (pHit, _phi, t) = (t, DifferentialGeometry pHit n) where
         n = normalize $ dpdu `cross` dpdv
         dpdu = mkV (-phimax * (vy pHit), phimax * (vx pHit), 0)
         dpdv = mkV (0, 0, zmax - zmin)

intersect (Sphere r) ray@(Ray ro rd tmin tmax)
   | isNothing times = Nothing
   | t1 > tmax = Nothing
   | t2 < tmin = Nothing
   | otherwise = Just (t, DifferentialGeometry hitPoint normalAt)
   where
         a = sqLen rd
         b = 2 * (ro `dot` rd)
         c = sqLen ro - (r * r)
         t = if t1 > tmin then t1 else t2
         (t1, t2) = fromJust times
         times = solveQuadric a b c
         hitPoint = rayAt ray t
         normalAt = normalize hitPoint

intersect (Triangle v1 v2 v3) r@(Ray ro rd tmin tmax)
   | divisor == 0 = Nothing
   | b1 < 0 || b1 > 1 = Nothing
   | b2 < 0 || b1 + b2 > 1 = Nothing
   | t < tmin || t > tmax = Nothing
   | otherwise = Just (t, DifferentialGeometry (rayAt r t) n)
   where
      n = normalize $ cross e2 e1
      t = dot e2 s2 * invDiv
      b2 = dot rd s2 * invDiv -- second barycentric
      s2 = cross d e1
      b1 = dot d s1 * invDiv -- first barycentric
      d = ro - p1
      invDiv = 1 / divisor
      divisor = dot s1 e1
      s1 = cross rd e2
      e1 = p2 - p1
      e2 = p3 - p1
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3

intersects :: Shape -> Ray -> Bool

intersects (Cylinder r zmin zmax phimax) ray@(Ray ro rd tmin tmax) =
   maybe False intersectsCylinder (solveQuadric a b c) where
      a = (vx rd) * (vx rd) + (vy rd) * (vy rd)
      b = 2 * ((vx rd) * (vx ro) + (vy rd) * (vy ro))
      c = (vx ro) * (vx ro) + (vy ro) * (vy ro) - r * r
      
      intersectsCylinder (t0, t1)
         | t0 > tmax = False
         | t1 < tmin = False
         | t > tmax = False
         | vz pHit0 > zmin && vz pHit0 < zmax && phi0 <= phimax = True
         | t == t1 = False
         | vz pHit1 > zmin && vz pHit1 < zmax && phi1 <= phimax = True
         | otherwise = False
         where
            t = if t0 > tmin then t0 else t1
            pHit0 = rayAt ray t0
            pHit1 = rayAt ray t1
            phi0 = atan2' (vy pHit0) (vx pHit0)
            phi1 = atan2' (vy pHit1) (vx pHit1)

intersects (Sphere rad) (Ray ro rd tmin tmax) = si where
   si = maybe False cb roots
   cb (t0, t1) -- check with ray bounds
      | t0 > tmax || t1 < tmin = False
      | t0 < tmin = t1 < tmax
      | otherwise = True
   a = sqLen rd
   b = 2 * dot ro rd
   c = sqLen ro - (rad * rad)
   roots = solveQuadric a b c
   
intersects (Triangle v1 v2 v3) (Ray ro rd tmin tmax)
   | divisor == 0 = False
   | b1 < 0 || b1 > 1 = False
   | b2 < 0 || b1 + b2 > 1 = False
   | t < tmin || t > tmax = False
   | otherwise = True
   where
      t = dot e2 s2 * invDiv
      b2 = dot rd s2 * invDiv -- second barycentric
      s2 = cross d e1
      b1 = dot d s1 * invDiv -- first barycentric
      d = ro - p1
      invDiv = 1 / divisor
      divisor = dot s1 e1
      s1 = cross rd e2
      e1 = p2 - p1
      e2 = p3 - p1
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3

-- | computes the world-space bounds of a shape
worldBounds :: Shape -- ^ the shape to get the world bounds for
            -> Transform -- ^ the transform placing the shape into world space
            -> AABB

-- for triangles, transform the vertices to world space and
-- compute the bounds of them
worldBounds (Triangle v1 v2 v3) t = foldl' extendAABBP emptyAABB pl where
      pl = map (transPoint t) pl'
      pl' = [vertexPos v1, vertexPos v2, vertexPos v3]
      
-- otherwise just transform the object bounds to world space
worldBounds s t = transBox t (objectBounds s)

-- | computes the bounding box of a shape in object space
objectBounds
   :: Shape -- ^ the shape to get the bounds for
   -> AABB -- ^ the bounds of the shape

objectBounds (Cylinder r z0 z1 _) = mkAABB p1 p2 where
   p1 = mkPoint nr nr z0
   p2 = mkPoint r r z1
   nr = -r
   
objectBounds (Sphere r) = mkAABB (mkPoint nr nr nr) (mkPoint r r r) where
   nr = -r

objectBounds (Triangle v1 v2 v3) = foldl' extendAABBP emptyAABB pl where
   pl = [vertexPos v1, vertexPos v2, vertexPos v3]

-- | computes the surface area of a @Shape@
area
   :: Shape -- ^ the @Shape@ to get the surface area for
   -> Flt -- ^ the surface area of that @Shape@

area (Cylinder r z0 z1 _) = 2 * pi * r * h where
   h = z1 - z0
   
area (Sphere r) = r * r * 4 * pi

area (Triangle v1 v2 v3) = 0.5 * len (cross (p2 - p1) (p3 - p1)) where
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3
      
insideSphere :: Flt -> Point -> Bool
insideSphere r pt = sqLen pt - r * r < 1e-4

pdf :: Shape -- ^ the @Shape@ to compute the pdf for
    -> Point -- ^ the point which is to be illuminated
    -> Vector -- ^ the wi vector
    -> Flt -- ^ the computed pdf value
    
pdf s p wi = maybe 0 p' (s `intersect` r) where
   r = Ray p wi 0 infinity
   f pd = if isInfinite pd then 0 else pd
   p' (t, dg) = f $ sqLen (p - (rayAt r t)) / (absDot (dgN dg) (-wi) * area s)

-- | the probability of choosing the specified point by sampling a @Shape@
pdf'
   :: Shape -- ^ the @Shape@ to get the pdf for
   -> Point -- ^ the @Point@ on that @Shape@
   -> Flt -- ^ the probability for choosing this @Point@

pdf' s _ = 1 / area s

-- | returns a random point (along with its normal) on the object, 
--   which is preferably visible from the specified point
sample :: Shape -> Point -> Rand2D -> (Point, Normal)

sample sp@(Sphere r) p us
   | insideSphere r p = sample' sp us -- sample full sphere if inside
   | otherwise = (ps, normalize ps) where -- sample only the visible part
      d = uniformSampleCone cs cosThetaMax us
      cs = coordinateSystem dn
      dn = normalize (-p)
      cosThetaMax = sqrt $ max 0 (1 - (r * r) / sqLen p)
      ps = maybe (dn * vpromote r) (\i -> rayAt ray (fst i)) int where
         ray = Ray p d 0 infinity
         int = sp `intersect` ray

sample s _ us = sample' s us -- ignore the point if nothing clever can be done

-- | returns a random Point and the Normal from a @Shape@
sample'
   :: Shape
   -> Rand2D
   -> (Point, Normal)

sample' (Cylinder r z0 z1 _) (u1, u2) = (p, n) where
   p = mkPoint (r * cos phi) (r * sin phi) z
   z = lerp u1 z0 z1
   phi = lerp u2 0 twoPi
   n = normalize $ mkV (vx p, vy p, 0)
   
sample' (Sphere r) us = (p * vpromote r, p) where
   p = uniformSampleSphere us 

sample' (Triangle v1 v2 v3) (u1, u2) = (p, n) where
   p = p1 * vpromote b1 + p2 * vpromote b2 + p3 * vpromote (1 - b1 - b2)
   (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
   b1 = 1 - u1' -- first barycentric
   b2 = u2 * u1' -- second barycentric
   u1' = sqrt u1
   n = normalize $ cross (p3 - p1) (p2 - p1)
