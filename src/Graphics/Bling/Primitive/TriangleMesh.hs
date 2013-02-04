
module Graphics.Bling.Primitive.TriangleMesh (

   mkTriangleMesh, triangulate
   
   ) where

import Data.List (foldl', tails)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Utils

import Debug.Trace

triangulate :: [[a]] -> [a]
triangulate = concatMap go where
   go [] = []
   go (f0:fs) = concatMap (f0:) $
                        map (take 2) $
                        takeWhile (\x -> length x >= 2) $
                        tails fs

data Mesh = Mesh
   { mvidx  :: ! (V.Vector Int)
   , mps    :: ! (V.Vector Point)
   , mns    :: ! (Maybe (V.Vector Normal))
   , muvs   :: ! (Maybe (V.Vector Float))
   , mmat   :: ! Material
   }

mkTriangleMesh
   :: Transform                        -- ^ object-to-world transform
   -> Material                         -- ^ material for all triangles
   -> V.Vector Point                   -- ^ vertex positions
   -> V.Vector Int                     -- ^ vertex indices for triangles
   -> Maybe (V.Vector Normal)          -- ^ shading normals
   -> Maybe (V.Vector (Float, Float))  -- ^ uv coordinates for parametrization
   -> Primitive
mkTriangleMesh o2w mat p i n uv
   | V.any (< 0) i = error
         "mkTriangleMesh: contains negative indices"
   | V.length i `rem` 3 /= 0 = error
         "mkTriangleMesh: number of indices must be multiple of 3"
   | V.any (>= V.length p) i = error
         "mkTriangleMesh: contains out of bounds indices"
   | maybe False (\uv' -> V.any (>= V.length uv') i) uv = error $
         "mkTriangleMesh: # of UVs and # of points mismatch (" ++ (show (V.length $ fromJust uv)) ++ ")"
   | otherwise = {-# SCC "mkTriangleMesh" #-} Primitive inter inters wb flat Nothing sg
   where
      p' = V.map (transPoint o2w) p
      n' = n >>= \ns -> return $ V.map (transNormal o2w) ns
      mesh = Mesh i p' n' (fmap flatTuple uv) mat
      flat = map (mkTri mesh) [0..(V.length i `quot` 3 - 1)]
      wb = V.foldl' extendAABBP emptyAABB p'
      inter _ = error "TriangleMesh : unimplemented intersects"
      inters _ = error "TriangleMesh : unimplemented intersects"
      sg = error "TriangleMesh : unimplemented shadingGeometry"
      
--------------------------------------------------------------------------------
-- Triangles
--------------------------------------------------------------------------------

-- data Triangle = Tri {-# UNPACK #-} ! Int ! TriangleMesh

triOffsets :: Int -> Mesh -> (Int, Int, Int)
{-# INLINE triOffsets #-}
triOffsets idx m = (o1, o2, o3) where
   o1 = V.unsafeIndex (mvidx m) (idx + 0)
   o2 = V.unsafeIndex (mvidx m) (idx + 1)
   o3 = V.unsafeIndex (mvidx m) (idx + 2)

triPoints :: Int -> Mesh -> (Point, Point, Point)
{-# INLINE triPoints #-}
triPoints idx m = (p1, p2, p3) where
   (o1, o2, o3) = triOffsets idx m
   p1 = V.unsafeIndex (mps m) o1
   p2 = V.unsafeIndex (mps m) o2
   p3 = V.unsafeIndex (mps m) o3

-- | assumes that the mesh actually *has* normals
triNormals :: Int -> Mesh -> (Normal, Normal, Normal)
{-# INLINE triNormals #-}
triNormals i m =
   let ns = fromJust (mns m)
       (o1, o2, o3) = triOffsets i m
   in
      (V.unsafeIndex ns o1, V.unsafeIndex ns o2, V.unsafeIndex ns o3)

triUVs :: Int -> Mesh -> (Float, Float, Float, Float, Float, Float)
{-# INLINE triUVs #-}
triUVs idx m = maybe (0, 0, 1, 0, 1, 1) go (muvs m) where
   (o1, o2, o3) = triOffsets idx m

   go uvs = (muv (2 * o1), muv (2 * o1 + 1),
             muv (2 * o2), muv (2 * o2 + 1),
             muv (2 * o3), muv (2 * o3 + 1)) where
             
         muv i = V.unsafeIndex uvs i             

mkTri :: Mesh -> Int -> Primitive
mkTri mesh n
   | V.length (mvidx mesh) <= (n * 3 + 2) = error "out of bounds triangle"
   | otherwise = prim where
      prim = Primitive inter inters wb flat Nothing sg
      idx = 3 * n
      inter ray = {-# SCC "intersect" #-} intersectTri prim idx mesh ray
      inters ray = {-# SCC "intersects" #-} intersectsTri idx mesh ray
      flat = [prim]
      
      wb = foldl' extendAABBP emptyAABB [p1, p2, p3] where
         (p1, p2, p3) = triPoints idx mesh

      sg o2w dgg
         | isNothing $ mns mesh = dgg
         | otherwise = dgg { dgN = ns, dgDPDU = ss, dgDPDV = ts }
         where
            (b1, b2) = fromJust $ dgtriB dgg
            b0 = 1 - b1 - b2
            (n0, n1, n2) = triNormals idx mesh
            ns = normalize $ transNormal o2w ns'
            ns' = (b0 *# n0) + (b1 *# n1) + (b2 *# n2)
            ss' = normalize $ dgDPDU dgg
            ts' = ss' `cross` ns
            (ss, ts) = if sqLen ts' > 0
                          then (normalize ts' `cross` ns, normalize ts')
                          else coordinateSystem'' ns

intersectsTri :: Int -> Mesh -> Ray -> Bool
intersectsTri idx mesh (Ray ro rd tmin tmax)
   | divisor == 0 = False
   | b1 < 0 || b1 > 1 = False
   | b2 < 0 || b1 + b2 > 1 = False
   | t < tmin || t > tmax = False
   | otherwise = True
   where
      (p1, p2, p3) = triPoints idx mesh
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

intersectTri :: Primitive -> Int -> Mesh -> Ray -> Maybe Intersection
intersectTri tri idx mesh r@(Ray ro rd tmin tmax)
   | divisor == 0 = Nothing
   | b1 < 0 || b1 > 1 = Nothing
   | b2 < 0 || b1 + b2 > 1 = Nothing
   | t < tmin || t > tmax = Nothing
   | otherwise = Just int
   where
      (p1, p2, p3) = triPoints idx mesh
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
      (uv00, uv01, uv10, uv11, uv20, uv21) = triUVs idx mesh
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
      tu = b0 * uv00 + b1 * uv10 + b2 * uv20
      tv = b0 * uv01 + b1 * uv11 + b2 * uv21
      
      -- create intersection
      dg = mkDgTri (rayAt r t) tu tv dpdu dpdv (mkV (0, 0, 0)) (mkV (0,0, 0)) (b1, b2)
      e = 1e-3 * t
      int = mkIntersection t e dg tri (mmat mesh)
      
