{-# LANGUAGE ExistentialQuantification #-}

module Geometry where

import Math

import Maybe

data Intersection = Intersection {
   intDist :: Float, -- ^ distance to this intersection
   intPos :: Point, -- ^ position of this intersection in world space
   intNorm :: Normal -- ^ surface normal at this intersection
   }

class Intersectable a where
   intersect :: Ray -> a -> Maybe Intersection
   intersects :: Ray -> a -> Bool

nearest :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
nearest mi1 mi2
   | isNothing mi1 = mi2
   | isNothing mi2 = mi1
   | otherwise = nearest' (fromJust mi1) (fromJust mi2)
   where
         nearest' i1@(Intersection d1 _ _) i2@(Intersection d2 _ _) =
            if (d1 < d2)
               then Just i1
               else Just i2
   
data AnyIntersectable = forall a. Intersectable a => MkAnyIntersectable a

instance Intersectable AnyIntersectable where
   intersect r (MkAnyIntersectable a) = intersect r a
   intersects r (MkAnyIntersectable a) = intersects r a

data Group = Group [AnyIntersectable]

instance Intersectable Group where
   intersect _ (Group []) = Nothing
   intersect r (Group g) = foldl nearest Nothing ints where
      ints = map (intersect r) g
      
   intersects _ (Group []) = False
   intersects r (Group (x:xs)) = intersects r x || intersects r (Group xs)
   
-- | a sphere has a radius and a position
data Sphere = Sphere Float Point

instance Intersectable Sphere where
   intersect ray@(Ray origin rayDir _ _) (Sphere r center)
      | times == [] = Nothing
      | otherwise = Just (Intersection time (hitPoint time) (normalAt time))
      where
         dir = origin `sub` center
         a = sqLen rayDir
         b = 2 * (rayDir `dot` dir)
         c = (sqLen dir) - (r * r)
         time = minimum times
         times = filter (> epsilon) (roots a b c)
         hitPoint = positionAt ray
         normalAt t = normalize (sub (hitPoint t) center)

   intersects (Ray ro rd _ _) (Sphere rad ct) = not $ null (filter (> epsilon) (roots a b c))
      where
         d = ro `sub` ct
         a = sqLen rd
         b = 2 * (d `dot` rd)
         c = (sqLen d) - (rad * rad)

-- | a plane has a distance from world-space origin and a normal
data Plane = Plane Float Normal

instance Intersectable Plane where
   intersect ray@(Ray ro rd _ _) (Plane d n)
      | t < epsilon = Nothing
      | otherwise = Just (Intersection t (positionAt ray t) n)
      where
         t = -(ro `dot` n + d) / (rd `dot` n)

   intersects (Ray ro rd _ _) (Plane d n) = ((ro `dot` n + d) / (rd `dot` n)) < 0
   