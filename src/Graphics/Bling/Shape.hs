
module Graphics.Bling.Shape (
   Shape,

   -- * Triangles and Meshes
   Vertex(..), triangulate,
   
   -- * Creating shapes
   
   mkCylinder, mkDisk, mkQuad, mkSphere, mkTriangle, mkBox,

   -- * Working with shapes
   
   area, sample, sample', pdf, pdf',
   objectBounds, worldBounds, intersect, intersects
   ) where

import Data.List (foldl')
import Data.Maybe

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Montecarlo
import Graphics.Bling.Random

data Vertex = Vertex {
   vertexPos :: {-# UNPACK #-} !Point
   } deriving (Eq, Show)   

data Shape
   = Box {-# UNPACK #-} !Point {-# UNPACK #-} !Point
      -- minimum and maximum extent
   | Cylinder
      {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt
      -- radius zmin zmax phimax
   | Disk
      {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt
      -- height, radius, inner radius, phimax
   | Quad
      {-# UNPACK #-} ! Flt {-# UNPACK #-} ! Flt
      -- size in the (x,y) plane
   | Sphere
      {-# UNPACK #-} !Flt
      -- radius
   | Triangle
      Vertex Vertex Vertex
      
   deriving (Eq, Show)

-- | creates a box with the specified extent
mkBox :: Point -> Point -> Shape
mkBox (Vector x0 y0 z0) (Vector x1 y1 z1) = Box pmin pmax where
   pmax = mkV (max x0 x1, max y0 y1, max z0 z1)
   pmin = mkV (min x0 x1, min y0 y1, min z0 z1)

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

-- | a disk parallel to the xy - plane
mkDisk
   :: Flt -- ^ offset from xy - plane (aka height)
   -> Flt -- ^ outer radius
   -> Flt -- ^ inner radius
   -> Flt -- ^ maximum phi angle in [degrees]
   -> Shape
mkDisk h r0 r1 mp = Disk h ro ri mp' where
   ro = max r0 r1
   ri = min r0 r1
   mp' = radians $ clamp mp 0 360

-- | creates a quad with the specified origin and size in the (x,y) plane
mkQuad :: Flt -> Flt-> Shape
mkQuad = Quad

-- | creates a sphere around the origin
mkSphere
   :: Flt -- ^ the sphere radius
   -> Shape
mkSphere = Sphere

mkTriangle
   :: (Vertex, Vertex, Vertex)
   -> Shape
mkTriangle (v1, v2, v3) = Triangle v1 v2 v3

triangulate :: [[Vertex]] -> [Shape]
triangulate vs = concatMap tr' vs where
   tr' (v1:v2:v3:xs) = Triangle v1 v2 v3 : tr' (v1:v3:xs)
   tr' _ = []

intersect :: Shape -> Ray -> Maybe (Flt, DifferentialGeometry)

intersect (Box pmin pmax) ray@(Ray o d tmin tmax) =
      testSlabs allDimensions tmin tmax 0 >>= go where
   
   go (t, axis) = Just (t, mkDg' p n) where
      p = rayAt ray t
      n = setComponent axis dir $ mkV (0, 0, 0)
      dir = if (p .! axis) > half then 1 else -1
      half = (pmin .! axis + pmax .! axis) / 2

   testSlabs :: [Dimension] -> Flt -> Flt -> Dimension -> Maybe (Flt, Dimension)
   testSlabs [] n f dd
      | n > f = Nothing
      | otherwise = Just (n, dd)
   testSlabs (dim:ds) near far dd
      | near > far = Nothing
      | otherwise = testSlabs ds (max near near') (min far far') (if near < near' then dim else dd) where
    (near', far') = if tNear > tFar then (tFar, tNear) else (tNear, tFar)
    tFar = (pmax .! dim - oc) * dInv
    tNear = (pmin .! dim - oc) * dInv
    oc = o .! dim
    dInv = 1 / d .! dim
    
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
      params (pHit, _phi, t) = (t, mkDg' pHit n) where
         n = normalize $ dpdu `cross` dpdv
         dpdu = mkV (-phimax * (vy pHit), phimax * (vx pHit), 0)
         dpdv = mkV (0, 0, zmax - zmin)

intersect (Disk h rad irad phimax) ray@(Ray ro rd tmin tmax)
   | abs (vz rd) < 1e-7 = Nothing -- parallel ray ?
   | t < tmin || t > tmax = Nothing -- distance in ray parameters ?
   | d2 > rad * rad || d2 < irad * irad = Nothing -- p inside disk radii ?
   | phi > phimax = Nothing
   | otherwise = Just (t, mkDg' p n)
   where
      t = (h - vz ro) / vz rd
      p = rayAt ray t
      (px, py) = (vx p, vy p)
      d2 = px * px + py * py
      phi = atan2' py px
      n = mkV (0, 0, -1)

intersect (Quad sx sz) ray@(Ray ro rd tmin tmax)
   | abs (vz rd) < 1e-7 = Nothing -- ray parallel to quad
   | t < tmin || t > tmax = Nothing -- ray parametric distance
   | abs (vx p) > sx || abs (vy p) > sz = Nothing -- not inside extent
   | otherwise = Just (t, mkDg' p (mkV (0, 0, -1)))
   where
      t = -(vz ro) / vz rd
      p = rayAt ray t

intersect (Sphere r) ray@(Ray ro rd tmin tmax)
   | isNothing times = Nothing
   | t1 > tmax = Nothing
   | t2 < tmin = Nothing
   | otherwise = Just (t, dg)
   where
      -- find hit point
      a = sqLen rd
      b = 2 * (ro `dot` rd)
      c = sqLen ro - (r * r)
      t = if t1 > tmin then t1 else t2
      (t1, t2) = fromJust times
      times = solveQuadric a b c
      
      -- find parametric representation of hit point
      thetaMin = pi
      thetaMax = 0
      phiMax = twoPi

      -- find dpdu and dpdv
      dg = mkDg p u v dpdu dpdv dndu dndv
      p@(Vector px py pz) = rayAt ray t
      phi = atan2' py px
      u = phi / phiMax
      theta = acos $ clamp (pz / r) (-1) 1
      v = (theta - thetaMin) / (thetaMax - thetaMin)
      zradius = sqrt $ px * px + py * py
      invzradius = 1 / zradius
      cosphi = px * invzradius
      sinphi = py * invzradius
      dpdu = mkV (-phiMax * py, phiMax * px, 0)
      dpdv = mkV (pz * cosphi, pz * sinphi, -r * sin theta) *
            (vpromote $ thetaMax - thetaMin)

      -- find dndu and dndv

      d2Pduu = vpromote (-phiMax * phiMax) * mkV (px, py, 0)
      d2Pduv = vpromote ((thetaMax - thetaMin) * pz * phiMax) *
                    mkV (-sinphi, cosphi, 0)
      d2Pdvv = vpromote (-(thetaMax - thetaMin) * (thetaMax - thetaMin)) *
                    mkV (px, py, pz);

      -- coefficients for fundamental forms
      e' = dpdu `dot` dpdu
      f' = dpdu `dot` dpdv
      g' = dpdv `dot` dpdv
      n' = normalize $ dpdu `cross` dpdv
      e = n' `dot` d2Pduu
      f = n' `dot` d2Pduv
      g = n' `dot` d2Pdvv
      invEGF2 = 1 / (e' * g' - f' * f')
      
      dndu = vpromote ((f*f' - e*g') * invEGF2) * dpdu +
             vpromote ((e*f' - f*e') * invEGF2) * dpdv
      dndv = vpromote ((g*f' - f*g') * invEGF2) * dpdu +
             vpromote ((f*f' - g*e') * invEGF2) * dpdv
      
intersect (Triangle v1 v2 v3) r@(Ray ro rd tmin tmax)
   | divisor == 0 = Nothing
   | b1 < 0 || b1 > 1 = Nothing
   | b2 < 0 || b1 + b2 > 1 = Nothing
   | t < tmin || t > tmax = Nothing
   | otherwise = Just (t, mkDg' (rayAt r t) n)
   where
      (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
      (e1, e2) = (p2 - p1, p3 - p1)
      s1 = rd `cross` e2
      divisor = dot s1 e1
      invDiv = 1 / divisor
      
      -- first barycentric
      b1 = dot d s1 * invDiv 
      d = ro - p1

      -- second barycentric
      b2 = dot rd s2 * invDiv
      s2 = cross d e1
      t = dot e2 s2 * invDiv
      n = normalize $ cross e1 e2
      
intersects :: Shape -> Ray -> Bool

intersects (Box pmin pmax) r = isJust $ intersectAABB (mkAABB pmin pmax) r

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

intersects (Disk h rad irad phimax) ray@(Ray ro rd tmin tmax)
   | abs (vz rd) < 1e-7 = False -- parallel ray ?
   | t < tmin || t > tmax = False -- distance in ray parameters ?
   | d2 > rad * rad || d2 < irad * irad = False -- p inside disk radii ?
   | phi > phimax = False
   | otherwise = True
   where
      t = (h - vz ro) / vz rd
      p = rayAt ray t
      (px, py) = (vx p, vy p)
      d2 = px * px + py * py
      phi = atan2' py px

intersects (Quad sx sy) ray@(Ray ro rd tmin tmax)
   | abs (vz rd) < 1e-7 = False -- ray parallel to quad
   | t < tmin || t > tmax = False -- ray parametric distance
   | abs (vx p) > sx || abs (vy p) > sy = False -- not inside extent
   | otherwise = True
   where
      t = -(vz ro) / vz rd
      p = rayAt ray t

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

objectBounds (Box p1 p2) = mkAABB p1 p2

objectBounds (Cylinder r z0 z1 _) = mkAABB p1 p2 where
   p1 = mkPoint' nr nr z0
   p2 = mkPoint' r r z1
   nr = -r

objectBounds (Disk h r _ _) = mkAABB p1 p2 where
   p1 = mkPoint' nr nr h
   p2 = mkPoint' r r h
   nr = -r

objectBounds (Quad sx sy) = mkAABB (mkPoint' (-sx) (-sy) 0) (mkPoint' sx sy 0)

objectBounds (Sphere r) = mkAABB (mkPoint' nr nr nr) (mkPoint' r r r) where
   nr = -r

objectBounds (Triangle v1 v2 v3) = foldl' extendAABBP emptyAABB pl where
   pl = [vertexPos v1, vertexPos v2, vertexPos v3]

-- | computes the surface area of a @Shape@
area
   :: Shape -- ^ the @Shape@ to get the surface area for
   -> Flt -- ^ the surface area of that @Shape@
area (Box pmin pmax) = 2 * (h*w + h*l + w*l) where
   (h, w, l) = (pmax .! dimX - pmin .! dimX,
                pmax .! dimY - pmin .! dimY,
                pmax .! dimZ - pmin .! dimZ)

area (Cylinder r z0 z1 _) = 2 * pi * r * h where
   h = z1 - z0
area (Disk _ rmax rmin _) = pi * (rmax2 - rmin2) where
   rmin2 = rmin * rmin
   rmax2 = rmax * rmax
area (Quad sx sz) = 2 * sx * sz
area (Sphere r) = r * r * 4 * pi
area (Triangle v1 v2 v3) = 0.5 * len ((p2 - p1) `cross` (p3 - p1)) where
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
   r = Ray p wi 1e-3 infinity
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

sample' (Box pmin pmax) (u1, u2) = (p, n) where
   (axis, u1') = remapRand 3 u1
   (nf, u2') = remapRand 2 u2
   n = setComponent axis ((fromIntegral nf * 2 - 1)) $ mkV (0, 0, 0)
   (oa0, oa1) = ((axis + 1) `mod` 3, (axis + 2) `mod` 3)
   
   p = setComponent oa0 (lerp u1' (pmin .! oa0) (pmax .! oa0)) $
       setComponent oa1 (lerp u2' (pmin .! oa1) (pmax .! oa1)) $
       if nf == 0 then pmin else pmax
   
sample' (Cylinder r z0 z1 _) (u1, u2) = (p, n) where
   p = mkPoint' (r * cos phi) (r * sin phi) z
   z = lerp u1 z0 z1
   phi = lerp u2 0 twoPi
   n = normalize $ mkV (vx p, vy p, 0)

sample' (Disk h rmax rmin phiMax) (u1, u2) = (p, mkV (0, 0, -1)) where
   p = mkPoint' (r * cos phi) (r * sin phi) h
   r = lerp u1 rmin rmax
   phi = lerp u2 0 phiMax

sample' (Quad sx sy) (u1, u2) = (p, mkV (0, 0, -1)) where
   p = mkPoint (lerp u1 (-sx) sx, lerp u2 (-sy) sy, 0)
   
sample' (Sphere r) us = (p * vpromote r, p) where
   p = uniformSampleSphere us 

sample' (Triangle v1 v2 v3) us = (p, n) where
   (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
   (b1, b2) = uniformSampleTriangle us
   p = p1 * vpromote b1 + p2 * vpromote b2 + p3 * vpromote (1 - b1 - b2)
   n = normalize $ (p2 - p1) `cross` (p3 - p1)
