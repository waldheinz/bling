{-# LANGUAGE ExistentialQuantification #-}

module Geometry where

import Math
import Maybe

data DifferentialGeometry = DifferentialGeometry {
   dgP :: Point,
   dgN :: Normal
   }

class Intersectable a where
   intersect :: Ray -> a -> Maybe (Float, DifferentialGeometry)
   intersects :: Ray -> a -> Bool

data AnyIntersectable = forall a. Intersectable a => MkAnyIntersectable a

instance Intersectable AnyIntersectable where
   intersect r (MkAnyIntersectable a) = intersect r a
   intersects r (MkAnyIntersectable a)  = intersects r a

-- | a sphere has a radius and a position
data Sphere = Sphere Float Point

instance Intersectable Sphere where
   intersect ray@(Ray origin rd _ _) (Sphere r center)
      | null times = Nothing
      | otherwise = Just $ (t, DifferentialGeometry (hitPoint t) (normalAt t))
      where
         dir = origin `sub` center
         a = sqLen rd
         b = 2 * (rd `dot` dir)
         c = (sqLen dir) - (r * r)
         t = minimum times
         times = filter (onRay ray) (roots a b c)
         hitPoint = positionAt ray
         normalAt tt = normalize $ sub (hitPoint tt) center

   intersects r@(Ray ro rd _ _) (Sphere rad ct) = not $ null (filter (onRay r) (roots a b c))
      where
         d = ro `sub` ct
         a = sqLen rd
         b = 2 * (d `dot` rd)
         c = (sqLen d) - (rad * rad)

-- | a plane has a distance from world-space origin and a normal
data Plane = Plane Float Normal

instance Intersectable Plane where
   intersect ray@(Ray ro rd _ _) (Plane d n)
      | not $ onRay ray t = Nothing
      | otherwise = Just $ (t, DifferentialGeometry (positionAt ray t) n)
      where
         t = -(ro `dot` n + d) / (rd `dot` n)
   
   intersects r@(Ray ro rd _ _) (Plane d n) = onRay r (-((ro `dot` n + d) / (rd `dot` n)))
   