
module Graphics.Bling.Primitive.TriangleMesh (

   TriangleMesh, mkTriangleMesh
   
   ) where

import Data.List (foldl')
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection

data TriangleMesh = Mesh
   { mvidx  :: V.Vector Int
   , mps    :: V.Vector Point
   , _mns    :: Maybe (V.Vector Normal)
   , _muvs   :: Maybe (V.Vector (Flt, Flt))
   , mmat   :: Material
   }

mkTriangleMesh
   :: Transform                     -- ^ object-to-world transform
   -> Material                      -- ^ material for all triangles
   -> V.Vector Point                -- ^ vertex positions
   -> V.Vector Int                  -- ^ vertex indices for triangles
   -> Maybe (V.Vector Normal)       -- ^ shading normals
   -> Maybe (V.Vector (Flt, Flt))   -- ^ uv coordinates for parametrization
   -> TriangleMesh
mkTriangleMesh o2w mat p i n uv
   | V.length i `rem` 3 /= 0 = error "mkTriangleMesh: number of indices must be multiple of 3"
   | V.any (>= V.length p) i = error "mkTriangleMesh: contains out of bounds indices"
   | otherwise = Mesh i p' n uv mat
   where
      p' = V.map (transPoint o2w) p
--      ntris = V.length i `div` 3
   
instance Primitive TriangleMesh where
   flatten mesh = map mkAnyPrim $ map (mkTri mesh) is where
      is = [0..((V.length $ mvidx mesh) `div` 3 - 1)]

   worldBounds mesh = V.foldl' extendAABBP emptyAABB $ mps mesh

   intersect _ _ = error "TriangleMesh : unimplemented intersects"
   intersects _ _ = error "TriangleMesh : unimplemented intersects"
   
--------------------------------------------------------------------------------
-- Triangles
--------------------------------------------------------------------------------

data Triangle = Tri {-# UNPACK #-} !Int ! TriangleMesh

mkTri :: TriangleMesh -> Int -> Triangle
mkTri mesh n
   | V.length (mvidx mesh) <= (n*3+2) = error "this triangle should not exist"
   | otherwise = Tri (n*3) mesh

triOffsets :: Triangle -> (Int, Int, Int)
{-# INLINE triOffsets #-}
triOffsets (Tri idx m) = (o1, o2, o3) where
   o1 = (mvidx m) V.! (idx + 0)
   o2 = (mvidx m) V.! (idx + 1)
   o3 = (mvidx m) V.! (idx + 2)

triPoints :: Triangle -> (Point, Point, Point)
{-# INLINE triPoints #-}
triPoints t@(Tri _ m) = (p1, p2, p3) where
   (o1, o2, o3) = triOffsets t
   p1 = (mps m) V.! o1
   p2 = (mps m) V.! o2
   p3 = (mps m) V.! o3

triMaterial :: Triangle -> Material
{-# INLINE triMaterial #-}
triMaterial (Tri _ m) = mmat m
   
instance Primitive Triangle where
   flatten tri = [mkAnyPrim tri]
   
   worldBounds tri = foldl' extendAABBP emptyAABB [p1, p2, p3] where
      (p1, p2, p3) = triPoints tri

   intersects tri (Ray ro rd tmin tmax)
      | divisor == 0 = False
      | b1 < 0 || b1 > 1 = False
      | b2 < 0 || b1 + b2 > 1 = False
      | t < tmin || t > tmax = False
      | otherwise = True
      where
         (p1, p2, p3) = triPoints tri
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

   intersect tri r@(Ray ro rd tmin tmax)
      | divisor == 0 = Nothing
      | b1 < 0 || b1 > 1 = Nothing
      | b2 < 0 || b1 + b2 > 1 = Nothing
      | t < tmin || t > tmax = Nothing
      | otherwise = Just int
      where
         (p1, p2, p3) = triPoints tri
         (e1, e2) = (p2 - p1, p3 - p1)
         s1 = rd `cross` e2
         divisor = dot s1 e1
         invDiv = 1 / divisor

         -- first barycentric
         b1 = dot d s1 * invDiv
         d = ro - p1

         -- second barycentric
         b2 = dot rd s2 * invDiv
         s2 = cross d e1
         t = dot e2 s2 * invDiv
         n = normalize $ cross e1 e2

         -- create intersection
         dg = mkDg' (rayAt r t) n
         int = Intersection t dg (mkAnyPrim tri) (triMaterial tri)
         