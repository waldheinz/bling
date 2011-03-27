
module Shape (
   Shape,
   mkSphere,
   area, sample, pdf
   ) where

import Data.Maybe

import AABB
import Math
import Random
import Transform

data Vertex = Vertex {
   vertexPos :: !Point
   } deriving (Eq, Show)   

data Shape
   = Sphere
      Flt -- ^ radius
   | Triangle Vertex Vertex Vertex
   deriving (Eq, Show)

mkSphere :: Flt -> Shape
mkSphere rad = Sphere rad

intersect :: Shape -> Ray -> Maybe (Flt, DifferentialGeometry)
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
         hitPoint = positionAt ray t
         normalAt = normalize hitPoint
         
intersects :: Shape -> Ray -> Bool
intersects (Sphere rad) (Ray ro rd tmin tmax)
   | isNothing roots = False
   | otherwise = t0 < tmax && t1 > tmin 
   where
         a = sqLen rd
         b = 2 * dot ro rd
         c = sqLen ro - (rad * rad)
         (t0, t1) = fromJust roots
         roots = solveQuadric a b c

objectBounds :: Shape -> AABB
objectBounds (Sphere r) = mkAABB (mkPoint nr nr nr) (mkPoint r r r) where
   nr = (-r)
   
area :: Shape -> Flt
area (Sphere r) = r * r * 4 * pi
area (Triangle v1 v2 v3) = 0.5 * len (cross (sub p2 p1) (sub p3 p1)) where
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3
      
insideSphere :: Flt -> Point -> Bool
insideSphere r pt = sqLen pt - r * r < 1e-4

pdf :: Shape -- ^ the @Shape@ to compute the pdf for
    -> Point -- ^ the point which is to be illuminated
    -> Vector -- ^ the wi vector
    -> Flt -- ^ the computed pdf value
    
pdf sp@(Sphere r) pos wi
   | insideSphere r pos = 1.0 / area sp
   | otherwise = uniformConePdf cosThetaMax
   where
      cosThetaMax = sqrt $ max 0 (1 - r * r / sqLen pos)
      
-- | returns a random point (along with its normal) on the object, 
--   which is preferably visible from the specified point
sample :: Shape -> Point -> Rand2D -> (Point, Normal)
sample sp@(Sphere r) p us
   | insideSphere r p = 
      let rndPt = randomOnSphere us 
      in (scalMul rndPt r, rndPt) -- sample full sphere if inside
      
   | otherwise = (ps, normalize ps) where -- sample only the visible part if outside
      d = uniformSampleCone cs cosThetaMax us
      cs = coordinateSystem dn
      dn = normalize p
      cosThetaMax = sqrt $ max 0 (1 - (r * r) / sqLen p)
      ps
         | isJust int = positionAt ray t
         | otherwise = scalMul dn r
         where
               ray = Ray p d 0 infinity
               int = intersect sp ray
               t = fst $ fromJust int

