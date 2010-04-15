module Geometry where

import Math

data Shape
  = Sphere Float Point --- a sphere has a radius and a position
  | Plane Float Normal -- a plane has a distance from origin and a normal
  | Group [Shape]
  
---
--- scene definition
---

--- a scene is a Shape (most probably a group) and some light sources
--data (Light' a) => Scene = Scene Shape [a]

--- extracts the lights from a scene
--sceneLights :: Scene -> [Light]
--sceneLights (Scene _ lights) = lights

--sceneShape :: Scene -> Shape
--sceneShape (Scene s _) = s

--- extracts the closest intersection from a list of intersections
closest :: [(Float, Intersection)] -> Intersection
closest xs = snd (foldl select (head xs) (tail xs))
  where
    select (t1, i1) (t2, i2)
      | t1 < t2 = (t1, i1)
      | otherwise = (t2, i2)
      
nearest :: Ray -> Shape -> Maybe Intersection
nearest r s
  | intersections == [] = Nothing
  | otherwise = Just (closest intersections)
  where
    intersections = intersect r s
      
--- determines all intersections of a ray and a shape
intersect :: Ray -> Shape -> [ (Float, Intersection) ]
intersect ray (Sphere r c) = intSphere ray r c
intersect ray (Group shapes) = intGroup ray shapes
intersect ray (Plane d n) = intPlane ray d n

intPlane :: Ray -> Float -> Normal -> [ (Float, Intersection) ]
intPlane ray@(ro, rd) d n
  | t < epsilon = []
  | otherwise = [ (t, (positionAt ray t, n, ray)) ]
  where
    t = -(ro `dot` n + d) / (rd `dot` n)

intSphere :: Ray -> Float -> Point -> [ (Float, Intersection) ]
intSphere ray@(origin, rayDir) r center = map (\t -> (t, intAt t)) times
  where
    dir = origin `sub` center
    a = sqLen rayDir
    b = 2 * (rayDir `dot` dir)
    c = (sqLen dir) - (r * r)
    times = filter (> epsilon) (roots a b c)
    hitPoint = positionAt ray
    intAt t = (hitPoint t, normalAt t, ray)
    normalAt t = normalize (sub (hitPoint t) center)

intGroup :: Ray -> [Shape] -> [ (Float, Intersection) ]
intGroup _ [] = []
intGroup ray (shape:rest) = (intersect ray shape) ++ (intGroup ray rest)

intersects :: Ray -> Shape -> Bool
intersects _ (Group []) = False
intersects r (Group (s:xs)) = (intersects r s) || intersects r (Group xs)
intersects r (Sphere rad center) = intersectsSphere r rad center
intersects (ro, rd) (Plane d n) = ((ro `dot` n + d) / (rd `dot` n)) < 0

intersectsSphere :: Ray -> Float -> Point -> Bool
intersectsSphere (ro, rd) rad ct = (filter (> epsilon) (roots a b c)) /= []
   where
         d = ro `sub` ct
         a = sqLen rd
         b = 2 * (d `dot` rd)
         c = (sqLen d) - (rad * rad)
