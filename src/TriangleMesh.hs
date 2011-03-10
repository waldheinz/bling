
module TriangleMesh (
   Vertex(..), TriangleMesh, mkMesh
   ) where

import AABB
import Geometry
import Material
import Math
import Primitive
import Transform

import Data.List (foldl')

data Vertex = Vertex {
   vertexPos :: !Point
   } deriving (Show)

vToWorld :: Transform -> Vertex -> Vertex
vToWorld t v = v { vertexPos = (transPoint t (vertexPos v)) }

data Triangle = Triangle {
   mesh :: TriangleMesh,
   vertex0 :: Vertex,
   vertex1 :: Vertex,
   vertex2 :: Vertex
   }

instance Geometry Triangle where
   boundArea (Triangle _ v1 v2 v3) = 0.5 * len (cross (sub p2 p1) (sub p3 p1)) where
      p1 = vertexPos v1
      p2 = vertexPos v2
      p3 = vertexPos v3
      
   boundSample (Triangle _ v1 v2 v3) _ (u1, u2) = (p, n) where
      p = scalMul p1 b1 `add` scalMul p2 b2 `add` scalMul p3 (1 - b1 - b2)
      (p1, p2, p3) = (vertexPos v1, vertexPos v2, vertexPos v3)
      b1 = 1 - u1' -- first barycentric
      b2 = u2 * u1' -- second barycentric
      u1' = sqrt u1
      n = normalize $ cross (sub p2 p1) (sub p3 p1)
      
   worldBounds (Triangle _ v1 v2 v3) = foldl' extendAABBP emptyAABB pl where
      pl = [vertexPos v1, vertexPos v2, vertexPos v3]
      
   localBounds (Triangle m v1 v2 v3) = foldl' extendAABBP emptyAABB pl where
      pl = map (transPoint (inverse (toWorld m))) pl'
      pl' = [vertexPos v1, vertexPos v2, vertexPos v3]
      
   intersect (Triangle _ v1 v2 v3) r@(Ray ro rd tmin tmax)
      | divisor == 0 = Nothing
      | b1 < 0 || b1 > 1 = Nothing
      | b2 < 0 || b1 + b2 > 1 = Nothing
      | t < tmin || t > tmax = Nothing
      | otherwise = Just (t, DifferentialGeometry (positionAt r t) n)
      where
            n = normalize $ cross e2 e1
            t = dot e2 s2 * invDiv
            b2 = dot rd s2 * invDiv -- second barycentric
            s2 = cross d e1
            b1 = dot d s1 * invDiv -- first barycentric
            d = sub ro p1
            invDiv = 1 / divisor
            divisor = dot s1 e1
            s1 = cross rd e2
            e1 = sub p2 p1
            e2 = sub p3 p1
            p1 = vertexPos v1
            p2 = vertexPos v2
            p3 = vertexPos v3

data TriangleMesh = MkMesh {
   mat :: Material,
   toWorld :: Transform,
   tris :: [Triangle]
   }

instance Prim TriangleMesh where
   primFlatten m = map MkAnyPrim ps where
      ps = map (\g -> mkPrim g (mat m) Nothing) (tris m)
      
   primWorldBounds m = transBox (toWorld m) $ foldl' extendAABB emptyAABB (map worldBounds (tris m))
   
   primIntersect = error "primIntersect TriangleMesh called"
   primIntersects = error "primIntersects TriangleMesh called"
   
triangulate :: TriangleMesh -> [Vertex] -> [Triangle]
triangulate m (v1:v2:v3:xs) = Triangle m v1 v2 v3 : triangulate m ([v1] ++ [v3] ++ xs)
triangulate m _ = []   
   
mkMesh :: Material -> Transform -> [[Vertex]] -> TriangleMesh
mkMesh m t vs = empty { tris = ts } where
   empty = MkMesh m t []
   ts = concatMap (triangulate empty) (map (map (vToWorld t)) vs)
   