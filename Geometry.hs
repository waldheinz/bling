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


nearestInt :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
nearestInt mi1 mi2
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

nearest :: (Intersectable i) => Ray -> [i] -> Maybe Intersection
nearest (Ray ro rd tmin tmax) i = nearest' i tmax Nothing where
 --  nearest' :: Ray -> [i] -> Float -> Maybe Intersection -> Maybe Intersection
   nearest' [] _ mi = mi
   nearest' (x:xs) tmax' mi = nearest' xs newMax newNear where
      clamped = (Ray ro rd tmin tmax')
      newNear = if (isJust newNear') then newNear' else mi
      newNear' = intersect clamped x
      newMax = if (isNothing newNear)
                  then tmax'
                  else intDist $ fromJust newNear

instance Intersectable Group where
   intersect _ (Group []) = Nothing
   intersect r (Group g) = nearest r g
      
   intersects _ (Group []) = False
   intersects r (Group (x:xs)) = intersects r x || intersects r (Group xs)
   
-- | a sphere has a radius and a position
data Sphere = Sphere Float Point

instance Intersectable Sphere where
   intersect ray@(Ray origin rd _ _) (Sphere r center)
      | null times = Nothing
      | otherwise = Just (Intersection time (hitPoint time) (normalAt time))
      where
         dir = origin `sub` center
         a = sqLen rd
         b = 2 * (rd `dot` dir)
         c = (sqLen dir) - (r * r)
         time = minimum times
         times = filter (onRay ray) (roots a b c)
         hitPoint = positionAt ray
         normalAt t = normalize $ sub (hitPoint t) center

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
      | otherwise = Just (Intersection t (positionAt ray t) n)
      where
         t = -(ro `dot` n + d) / (rd `dot` n)
   
   intersects r@(Ray ro rd _ _) (Plane d n) = onRay r (-((ro `dot` n + d) / (rd `dot` n)))
   