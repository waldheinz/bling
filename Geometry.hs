{-# LANGUAGE ExistentialQuantification #-}

module Geometry where

import Math
import Random

import Debug.Trace
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

sampleSphere :: Point -> Point -> Point
sampleSphere co pt
   | isNothing roots = pt
   | otherwise = normalize $ chosen
      where
         chosen = add co $ scalMul rd t
         rd = pt `sub` co
         (t, _) = fromJust roots
         roots = solveQuadric a b c
         a = sqLen rd
         b = 2 * (co `dot` rd)
         c = sqLen co - 1
   
instance Bound Sphere where
   boundArea (Sphere r _) = r * r * 4 * pi
   
   boundSample sp@(Sphere r center) p
      | insideSphere sp p = do -- sample full sphere if inside
         rndPt <- randomOnSphere
         return $! (add center $ scalMul rndPt r, rndPt)
         
      | otherwise = do -- sample only the visible part if outside
         d <- uniformSampleCone cs cosThetaMax
         return $! (ps d, normalize $ sub (ps d) center)
         where
               cs = coordinateSystem dn
               dn = normalize $ sub center p
               cosThetaMax = sqrt $ max 0 (1 - r * r / (sqLen $ sub p center))
               
               ps d
                  | isJust int = positionAt ray $ fst $ fromJust int
                  | otherwise = sub center $ scalMul dn r
                     where 
                           ray = Ray p d 0 infinity
                           int = intersect ray sp
         
   boundPdf sp@(Sphere r center) pos _
      | insideSphere sp pos = 1.0 / boundArea sp
      | otherwise = uniformConePdf cosThetaMax
      where
         cosThetaMax = sqrt $ max 0 (1 - r * r / (sqLen $ sub pos center))

debug :: Show a => a -> a
debug x = trace (show x) x

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

   intersects (Ray ro rd tmin tmax) (Sphere rad sc)
      | isNothing roots = False
      | otherwise = t0 < tmax && t1 > tmin
      where
            dst = ro `sub` sc
            a = sqLen rd
            b = 2 * dot dst rd
            rad' = rad - epsilon
            c = sqLen dst - (rad' * rad')
            (t0, t1) = fromJust roots
            roots = solveQuadric a b c
            
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
   