
module Graphics.Bling.Shape (
   Shape,

   -- * Triangles and Meshes
   Vertex(..), triangulate,
   
   -- * Creating shapes
   
   mkSphere,

   -- * Working with shapes
   
   area, sample, pdf, objectBounds, worldBounds, intersect, intersects
   ) where

import Data.List (foldl')
import Data.Maybe
import Debug.Trace

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Random
import Graphics.Bling.Montecarlo
import Graphics.Bling.Transform

data Vertex = Vertex {
   vertexPos :: {-# UNPACK #-} !Point
   } deriving (Eq, Show)   

data Shape
   = Sphere
      {-# UNPACK #-} !Flt -- ^ radius
   | Triangle Vertex Vertex Vertex
   deriving (Eq, Show)

-- | creates a sphere
mkSphere :: Flt -> Shape
mkSphere = Sphere

triangulate :: [[Vertex]] -> [Shape]
triangulate vs = concatMap tr' vs where
   tr' (v1:v2:v3:xs) = Triangle v1 v2 v3 : tr' (v1:v3:xs)
   tr' _ = []
   
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
         hitPoint = rayAt ray t
         normalAt = normalize hitPoint

intersect (Triangle v1 v2 v3) r@(Ray ro rd tmin tmax)
   | divisor == 0 = Nothing
   | b1 < 0 || b1 > 1 = Nothing
   | b2 < 0 || b1 + b2 > 1 = Nothing
   | t < tmin || t > tmax = Nothing
   | otherwise = Just (t, DifferentialGeometry (rayAt r t) n)
   where
      n = normalize $ cross e2 e1
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

intersects :: Shape -> Ray -> Bool
intersects (Sphere rad) (Ray ro rd tmin tmax) = si where
   si = maybe False (\(t0, t1) -> t0 < tmax && t1 > tmin ) roots
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
   
-- as a general way we can just check if @intersect@ returns something
-- intersects s r = isJust (s `intersect` r)

worldBounds :: Shape -- ^ the shape to get the world bounds for
            -> Transform
            -> AABB

-- for triangles, transform the vertices to world space and
-- compute the bounds of them
worldBounds (Triangle v1 v2 v3) t = foldl' extendAABBP emptyAABB pl where
      pl = map (transPoint t) pl'
      pl' = [vertexPos v1, vertexPos v2, vertexPos v3]
      
-- otherwise just transform the object bounds to world space
worldBounds s t = transBox t (objectBounds s)

objectBounds :: Shape -> AABB
objectBounds (Sphere r) = mkAABB (mkPoint nr nr nr) (mkPoint r r r) where
   nr = -r

objectBounds (Triangle v1 v2 v3) = foldl' extendAABBP emptyAABB pl where
   pl = [vertexPos v1, vertexPos v2, vertexPos v3]
   
area :: Shape -> Flt
area (Sphere r) = r * r * 4 * pi
area (Triangle v1 v2 v3) = 0.5 * len (cross (p2 - p1) (p3 - p1)) where
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3
      
insideSphere :: Flt -> Point -> Bool
insideSphere r pt = sqLen pt - r * r < 1e-4

pdf :: Shape -- ^ the @Shape@ to compute the pdf for
    -> Point -- ^ the point which is to be illuminated
    -> Vector -- ^ the wi vector
    -> Flt -- ^ the computed pdf value
    
pdf s p wi = maybe 0 pdf' (s `intersect` r) where
   r = Ray p wi 0 infinity
   f p = if isInfinite p then 0 else p
   pdf' (t, dg) = f $ sqLen (p - (rayAt r t)) / (absDot (dgN dg) (-wi) * area s)
   
pdf sp@(Sphere r) pos _
   | insideSphere r pos = 1 / area sp
   | otherwise = uniformConePdf cosThetaMax
   where
      cosThetaMax = sqrt $ max 0 (1 - r * r / sqLen pos)

-- for general shapes just return the inverse of the area
pdf s _ _ = 1 / area s

-- | returns a random point (along with its normal) on the object, 
--   which is preferably visible from the specified point
sample :: Shape -> Point -> Rand2D -> (Point, Normal)
sample sp@(Sphere r) p us
   | insideSphere r p = 
      let rndPt = randomOnSphere us 
      in (rndPt * vpromote r, rndPt) -- sample full sphere if inside
      
   | otherwise = (ps, normalize ps) where -- sample only the visible part if outside
      d = uniformSampleCone cs cosThetaMax us
      cs = coordinateSystem dn
      dn = normalize p
      cosThetaMax = sqrt $ max 0 (1 - (r * r) / sqLen p)
      ps = maybe (dn * vpromote r) (\i -> rayAt ray (fst i)) int where
         ray = Ray p d 0 infinity
         int = sp `intersect` ray
   
sample (Triangle v1 v2 v3) _ (u1, u2) = (p, n) where
   p = p1 * vpromote b1 + p2 * vpromote b2 + p3 * vpromote (1 - b1 - b2)
   (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
   b1 = 1 - u1' -- first barycentric
   b2 = u2 * u1' -- second barycentric
   u1' = sqrt u1
   n = normalize $ cross (p3 - p1) (p2 - p1)
      
