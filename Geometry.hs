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

instance Bound Sphere where
   boundArea (Sphere r _) = r * r * 4 * pi
   boundSample (Sphere r p) _ = do
      pt <- randomOnSphere
      return $! (add p $ scalMul pt r, pt)

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
   