
module TriangleMesh (Vertex(..), Triangle, triangulate) where

import AABB
import Geometry
import Math

data Vertex = Vertex {
   vertexPos :: !Point
   } deriving (Show)

data Triangle = Triangle Vertex Vertex Vertex deriving (Show)

instance Geometry Triangle where
   boundArea (Triangle v1 v2 v3) = 0.5 * (len $ cross (sub p2 p1) (sub p3 p1)) where
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3
      
   boundSample (Triangle v1 v2 v3) _ (u1, u2) = (p, n) where
      p = (scalMul p1 b1) `add` (scalMul p2 b2 ) `add` (scalMul p3 (1 - b1 - b2))
      (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
      b1 = 1 - u1' -- first barycentric
      b2 = u2 * u1' -- second barycentric
      u1' = sqrt u1
      n = normalize $ cross (sub p2 p1) (sub p3 p1)
      
   bounds (Triangle v1 v2 v3) = foldl extendAABBP emptyAABB [p1, p2, p3] where
      (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
      
   intersect (Triangle v1 v2 v3) r@(Ray ro rd tmin tmax)
      | divisor == 0 = Nothing
      | b1 < 0 || b1 > 1 = Nothing
      | b2 < 0 || b1 + b2 > 1 = Nothing
      | t < tmin || t > tmax = Nothing
      | otherwise = Just (t, DifferentialGeometry (positionAt r t) n)
      where
            n = normalize $ cross e2 e1
            t = (dot e2 s2) * invDiv
            b2 = (dot rd s2) * invDiv -- second barycentric
            s2 = cross d e1
            b1 = (dot d s1) * invDiv -- first barycentric
            d = sub ro p1
            invDiv = 1 / divisor
            divisor = dot s1 e1
            s1 = cross rd e2
            e1 = sub p2 p1
            e2 = sub p3 p1
            p1 = vertexPos v1
            p2 = vertexPos v2
            p3 = vertexPos v3

triangulate :: [Vertex] -> [Triangle]
triangulate (v1:v2:v3:xs) = [Triangle v1 v2 v3] ++ (triangulate $ [v1] ++ [v3] ++ xs)
triangulate _ = []
