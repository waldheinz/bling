
module Graphics.Bling.Primitive.TriangleMesh (

   TriangleMesh, mkTriangleMesh, triangulate
   
   ) where

import Data.List (foldl', tails)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection

data TriangleMesh = Mesh
   { mvidx  :: ! (V.Vector Int)
   , mps    :: ! (V.Vector Point)
   , mns    :: ! (Maybe (V.Vector Normal))
   , muvs   :: ! (Maybe (V.Vector Float))
   , mmat   :: ! Material
   }

mkTriangleMesh
   :: Transform                     -- ^ object-to-world transform
   -> Material                      -- ^ material for all triangles
   -> V.Vector Point                -- ^ vertex positions
   -> V.Vector Int                  -- ^ vertex indices for triangles
   -> Maybe (V.Vector Normal)       -- ^ shading normals
   -> Maybe (V.Vector Float)          -- ^ uv coordinates for parametrization
   -> TriangleMesh
mkTriangleMesh o2w mat p i n uv
   | V.length i `rem` 3 /= 0 = error "mkTriangleMesh: number of indices must be multiple of 3"
   | V.any (>= V.length p) i = error "mkTriangleMesh: contains out of bounds indices"
   | V.any (< 0) i = error "mkTriangleMesh: contains negative indices"
   | otherwise = Mesh i p' n' uv mat
   where
      p' = V.map (transPoint o2w) p
      n' = n >>= \ns -> return $ V.map (transNormal o2w) ns

triangulate :: [[Int]] -> [Int]
triangulate = concatMap go where
   go [] = []
   go (f0:fs) = concatMap (f0:) $
                        map (take 2) $
                        takeWhile (\x -> length x >= 2) $
                        tails fs

instance Primitive TriangleMesh where
   flatten mesh = map (mkAnyPrim . mkTri mesh) is where
      is = [0..(V.length (mvidx mesh) `div` 3 - 1)]

   worldBounds mesh = V.foldl' extendAABBP emptyAABB $ mps mesh

   intersect _ _ = error "TriangleMesh : unimplemented intersects"
   intersects _ _ = error "TriangleMesh : unimplemented intersects"
   
--------------------------------------------------------------------------------
-- Triangles
--------------------------------------------------------------------------------

data Triangle = Tri {-# UNPACK #-} !Int ! TriangleMesh

mkTri :: TriangleMesh -> Int -> Triangle
mkTri mesh n
   | V.length (mvidx mesh) <= (n*3+2) = error "this triangle does not exist"
   | otherwise = Tri (n*3) mesh

triOffsets :: Triangle -> (Int, Int, Int)
{-# INLINE triOffsets #-}
triOffsets (Tri idx m) = (o1, o2, o3) where
   o1 = mvidx m V.! (idx + 0)
   o2 = mvidx m V.! (idx + 1)
   o3 = mvidx m V.! (idx + 2)

triPoints :: Triangle -> (Point, Point, Point)
{-# INLINE triPoints #-}
triPoints t@(Tri _ m) = (p1, p2, p3) where
   (o1, o2, o3) = triOffsets t
   p1 = mps m V.! o1
   p2 = mps m V.! o2
   p3 = mps m V.! o3

-- | assumes that the mesh actually *has* normals
triNormals :: Triangle -> (Normal, Normal, Normal)
{-# INLINE triNormals #-}
triNormals t@(Tri _ m) = (ns V.! o1, ns V.! o2, ns V.! o3) where
   ns = fromJust (mns m)
   (o1, o2, o3) = triOffsets t

triMaterial :: Triangle -> Material
{-# INLINE triMaterial #-}
triMaterial (Tri _ m) = mmat m

triUVs :: Triangle -> (Float, Float, Float, Float, Float, Float)
{-# INLINE triUVs #-}
triUVs tri@(Tri _ m) = maybe (0, 0, 1, 0, 1, 1) go (muvs m) where
   (o1, o2, o3) = triOffsets tri
   muv x i = x V.! i
   go uvs = (muv uvs (2 * o1), muv uvs (2 * o1 + 1),
             muv uvs (2 * o2), muv uvs (2 * o2 + 1),
             muv uvs (2 * o3), muv uvs (2 * o3 + 1))
   
instance Primitive Triangle where
   flatten tri = [mkAnyPrim tri]
   
   worldBounds tri = foldl' extendAABBP emptyAABB [p1, p2, p3] where
      (p1, p2, p3) = triPoints tri

   intersect t r = {-# SCC "intersect" #-} intersectTri t r
   intersects t r = {-# SCC "intersects" #-} intersectsTri t r
   
   shadingGeometry tri@(Tri _ mesh) o2w dgg
      | isNothing $ mns mesh = dgg
      | otherwise = dgg { dgN = ns, dgDPDU = ss, dgDPDV = ts }
      where
         (b0, b1, b2) = (1 - b1 - b2, dgU dgg, dgV dgg)
         (n0, n1, n2) = triNormals tri
         ns = normalize $ transNormal o2w ns'
         ns' = (b0 *# n0) + (b1 *# n1) + (b2 *# n2)
         ss' = normalize $ dgDPDU dgg
         ts' = ss' `cross` ns
         (ss, ts) = if sqLen ts' > 0
                       then (normalize ts' `cross` ns, normalize ts')
                       else coordinateSystem'' ns

intersectsTri :: Triangle -> Ray -> Bool
intersectsTri tri (Ray ro rd tmin tmax)
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

intersectTri :: Triangle -> Ray -> Maybe Intersection
intersectTri tri r@(Ray ro rd tmin tmax)
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

      -- compute partial derivatives
      (uv00, uv01, uv10, uv11, uv20, uv21) = triUVs tri
      du1 = uv00 - uv20
      du2 = uv10 - uv20
      dv1 = uv01 - uv21
      dv2 = uv11 - uv21
      (dp1, dp2) = (p1 - p3, p2 - p3)
      determinant = du1 * dv2 - dv1 * du2

      (dpdu, dpdv)
         | determinant == 0 = coordinateSystem'' n
         | otherwise = (invDet *# (  dv2  *# dp1 - dv1 *# dp2),
                        invDet *# ((-du2) *# dp1 + du1 *# dp2))
         where
            invDet = 1 / determinant

      -- interpolate u and v
      b0 = 1 - b1 - b2
      tu = b1 -- b0 * uv00 + b1 * uv10 + b2 * uv20
      tv = b2 -- b0 * uv01 + b1 * uv11 + b2 * uv21
         
      -- create intersection
      dg = mkDg (rayAt r t) tu tv dpdu dpdv (mkV (0, 0, 0)) (mkV (0,0, 0))
      e = 1e-3 * t
      int = Intersection t e dg (mkAnyPrim tri) (triMaterial tri)
         
