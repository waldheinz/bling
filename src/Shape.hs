
module Shape (
   Shape,
   DifferentialGeometry, dgP, dgN,
   mkSphere,
   area, intersect, intersects, objectBounds
   ) where

import Data.Maybe

import AABB
import Math
import Transform

data DifferentialGeometry = DifferentialGeometry {
   dgP :: Point,
   dgN :: Normal
   } deriving (Show)
   
-- | transforms a @DifferentialGeometry@ to world space
transDg :: ShapeBase -> DifferentialGeometry -> DifferentialGeometry
transDg (MkSB t _ _) (DifferentialGeometry p n) =
   DifferentialGeometry (transPoint t p) (transNormal t n)

data ShapeBase = MkSB {
   o2w :: Transform, -- ^ the object-to-world transformation
   w2o :: Transform, -- ^ the world-to-object transformation
   reverseOrientation :: Bool -- ^ reverse the normal orientation?
   } deriving (Eq, Show)

mkSb :: Transform -> Bool -> ShapeBase
mkSb t ro = MkSB t (inverse t) ro

data Shape
   = Sphere
      ShapeBase -- ^ position
      Flt -- ^ radius
      
   deriving (Eq, Show)

mkSphere :: Transform -> Bool -> Flt -> Shape
mkSphere t o rad = Sphere (mkSb t o) rad

intersect :: Shape -> Ray -> Maybe (Flt, DifferentialGeometry)
intersect (Sphere sb r) ray
   | isNothing times = Nothing
   | t1 > tmax = Nothing
   | t2 < tmin = Nothing
   | otherwise = Just (t, transDg sb dg)
   where
         (Ray ro rd tmin tmax) = transRay (w2o sb) ray
         a = sqLen rd
         b = 2 * (ro `dot` rd)
         c = sqLen ro - (r * r)
         t = if t1 > tmin then t1 else t2
         (t1, t2) = fromJust times
         times = solveQuadric a b c
         dg = DifferentialGeometry hitPoint normalAt
         hitPoint = positionAt ray t
         normalAt = normalize hitPoint
         
intersects :: Shape -> Ray -> Bool
intersects (Sphere sb rad) ray
   | isNothing roots = False
   | otherwise = t0 < tmax && t1 > tmin 
   where
         (Ray ro rd tmin tmax) = transRay (w2o sb) ray
         a = sqLen rd
         b = 2 * dot ro rd
         c = sqLen ro - (rad * rad)
         (t0, t1) = fromJust roots
         roots = solveQuadric a b c

objectBounds :: Shape -> AABB
objectBounds (Sphere _ r) = mkAABB (mkPoint nr nr nr) (mkPoint r r r) where
   nr = (-r)

area :: Shape -> Flt
area (Sphere _ r) = r * r * 4 * pi
