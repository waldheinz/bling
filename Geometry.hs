{-# LANGUAGE ExistentialQuantification #-}

module Geometry where

import Math
import Random

import Maybe

data DifferentialGeometry = DifferentialGeometry {
   dgP :: Point,
   dgN :: Normal
   }

class Intersectable a where
   intersect :: Ray -> a -> Maybe (Float, DifferentialGeometry)
   intersects :: Ray -> a -> Bool

class (Intersectable a) => Bound a where
   boundArea :: a -> Float -- ^ the area of the object
   boundSample :: a -> Point -> Rand (Point, Normal) -- ^ a random point (along with its normal) on the object, which is preferably visible from the given point
   boundPdf :: a -> Point -> Vector -> Float
   
   boundPdf a _ _ = 1.0 / boundArea a
   
-- | a sphere has a radius and a position
data Sphere = Sphere Float Point

insideSphere :: Sphere -> Point -> Bool
insideSphere (Sphere r pos) pt = (sqLen $ sub pos pt) - r * r < epsilon

instance Bound Sphere where
   boundArea (Sphere r _) = r * r * 4 * pi
   
   boundSample sp@(Sphere r pos) p
      | insideSphere sp p = do -- sample full sphere if inside
         rndPt <- randomOnSphere
         return $! (add pos $ scalMul rndPt r, rndPt)
         
      | otherwise = do -- sample only the visible part if outside
         rndPt <-randomOnSphere
         return $! (add pos (scalMul (ptFlip rndPt) r), (ptFlip rndPt)) where
            ptFlip rndPt -- possbily flips the point to the other side of unit sphere
               | (dot rndPt $ normalize $ sub p pos) < 0 = neg rndPt
               | otherwise = rndPt
         
   boundPdf sp p _
      | insideSphere sp p = 1.0 / boundArea sp
      | otherwise = 1.0 / boundArea sp

instance Intersectable Sphere where
   intersect ray@(Ray origin rd tmin tmax) (Sphere r center)
      | isNothing times = Nothing
      | t1 > tmax = Nothing
      | t2 < tmin = Nothing
      | otherwise = Just $ (t, DifferentialGeometry hitPoint normalAt)
      where
         co = origin `sub` center
         a = sqLen rd
         b = 2 * (co `dot` rd)
         c = (sqLen co) - (r * r)
         t = if (t1 > tmin) then t1 else t2
         (t1, t2) = fromJust times
         times = solveQuadric a b c
         hitPoint = positionAt ray t
         normalAt = normalize $ sub hitPoint center

   intersects ray@(Ray ro rd tmin tmax) (Sphere rad sc)
      | isNothing roots = False
      | otherwise = onRay ray t0 || onRay ray t1
      where
            dst = ro `sub` sc
            a = sqLen rd
            b = 2 * dot dst rd
            c = sqLen dst - (rad * rad)
            (t0, t1) = fromJust roots
            roots = solveQuadric a b c
{-
   intersects (Ray (rox, roy, roz) (rdx, rdy, rdz) rmin rmax) (Sphere rad ct)
      | isNothing ts = False
      | t0 > rmax = False
      | t1 < rmin = False
      | otherwise = True
         where 
               (t0, t1) = fromJust ts
               ts = roots a b c
               a = rdx*rdx + rdy*rdy + rdz*rdz
               b = 2 * (rdx*rox + rdy*roy + rdz*roz)
               c = rox*rox + roy*roy + roz*roz - rad*rad;
-}
-- | a plane has a distance from world-space origin and a normal
data Plane = Plane Float Normal

instance Intersectable Plane where
   intersect ray@(Ray ro rd _ _) (Plane d n)
      | not $ onRay ray t = Nothing
      | otherwise = Just $ (t, DifferentialGeometry (positionAt ray t) n)
      where
         t = -(ro `dot` n + d) / (rd `dot` n)
   
   intersects r@(Ray ro rd _ _) (Plane d n) = onRay r (-((ro `dot` n + d) / (rd `dot` n)))
   
data AnyIntersectable = forall a. Intersectable a => MkAnyIntersectable a

instance Intersectable AnyIntersectable where
   intersect r (MkAnyIntersectable a) = intersect r a
   intersects r (MkAnyIntersectable a)  = intersects r a

data AnyBound = forall a. Bound a => MkAnyBound a

instance Intersectable AnyBound where
   intersect r (MkAnyBound a) = intersect r a
   intersects r (MkAnyBound a)  = intersects r a

instance Bound AnyBound where
   boundArea (MkAnyBound a) = boundArea a
   boundSample (MkAnyBound a) p = boundSample a p
   