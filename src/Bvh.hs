
module Bvh 
   ( Bvh, mkBvh, ppBvh ) 
   where

import Data.Maybe (isJust, fromJust)
import Text.PrettyPrint

import AABB
import Math
import Primitive

type Bvh = TreeBvh
   
--
-- The simple "tree" BVH implementation
--

data TreeBvh
   = Node
      {-# UNPACK #-} !Dimension
      !TreeBvh
      !TreeBvh
      {-# UNPACK #-} !AABB
   | Leaf 
      ![AnyPrim]
      {-# UNPACK #-} !AABB

--
-- pretty printing BVH stats
---

ppBvh :: TreeBvh -> Doc
ppBvh t = vcat [
   text "primitive count" <+> (int p),
   text "maximum depth" <+> (int md),
   text "maximum leaf prims" <+> (int (maxPrims t)),
   text "number of leaves" <+> (int l),
   text "avg. depth" <+> (float ((fromIntegral sd) / (fromIntegral l))),
   text "avg. prims per leaf" <+> (float ((fromIntegral p) / (fromIntegral l)))
   ] where
      (md, sd) = maxDepth t
      l = leafCount t
      p = primCount t

primCount :: TreeBvh -> Int
primCount (Leaf ps _) = length ps
primCount (Node _ l r _) = (primCount l) + (primCount r)

maxPrims :: TreeBvh -> Int
maxPrims (Leaf ps _) = length ps
maxPrims (Node _ l r _) = max (maxPrims l) (maxPrims r)

maxDepth :: TreeBvh -> (Int, Int)
maxDepth t = maxDepth' t (0, 0) where
   maxDepth' (Leaf _ _) (m, s) = (m + 1, s + 1)
   maxDepth' (Node _ l r _) (m, s) = (max ml mr, sl + sr) where
      (ml, sl) = maxDepth' l (m + 1, s + 1)
      (mr, sr) = maxDepth' r (m + 1, s + 1)

leafCount :: TreeBvh -> Int
leafCount t = leafCount' t where
   leafCount' (Leaf _ _) = 1
   leafCount' (Node _ l r _) = (leafCount' l) + (leafCount' r)

instance Primitive TreeBvh where
   intersects = bvhIntersects
   intersect = bvhIntersect
   worldBounds (Node _ _ _ b) = b
   worldBounds (Leaf _ b) = b
   flatten _ = error "Unimplemented BVH.flatten"
   
mkBvh :: [AnyPrim] -> TreeBvh
mkBvh [] = Leaf [] emptyAABB
mkBvh [p] = Leaf [p] $ worldBounds p
mkBvh ps
   | null left = Leaf right allBounds
   | null right = Leaf left allBounds
   | otherwise = Node dim (mkBvh left) (mkBvh right) allBounds where
   (left, right) = splitMidpoint ps dim
   dim = splitAxis ps
   allBounds = foldl extendAABB emptyAABB $ map worldBounds ps

bvhIntersect :: TreeBvh -> Ray -> Maybe Intersection
bvhIntersect bvh ray@(Ray ro rd tmin tmax) = go bvh where
   i = intf ray
   go (Leaf p b) = if i b then nearest ray p else Nothing
   go (Node d l r b) = if i b then near firstInt otherInt else Nothing where
      (firstChild, otherChild) = if component rd d > 0 then (l, r) else (r, l)
      firstInt = bvhIntersect firstChild ray
      tmax'
         | isJust firstInt = intDist (fromJust firstInt)
         | otherwise = tmax
      otherInt = bvhIntersect otherChild (Ray ro rd tmin tmax')

near :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
near Nothing i = i
near i Nothing = i
near mi1 mi2 = Just $ near' (fromJust mi1) (fromJust mi2) where
   near' i1@(Intersection d1 _ _ _) i2@(Intersection d2 _ _ _)
      | d1 < d2 = i1
      | otherwise = i2

-- | creates an itersection function which can be used to check multiple
--   AABBs for intersection against a single @Ray@
intf :: Ray -> (AABB -> Bool)
{-# INLINE intf #-}
intf ray@(Ray _ (MkVector dx dy dz) _ _) = (\b -> intAABB b ray invD negD) where
   invD = mkV (1 / dx, 1 / dy, 1 / dz)
   negD = (isNeg dx, isNeg dy, isNeg dz)
   isNeg x = if x < 0 then 1 else 0
   
bvhIntersects :: TreeBvh -> Ray -> Bool
bvhIntersects bvh ray = go bvh where
   go (Leaf p b) = i b && any (flip intersects ray) p
   go (Node _ l r b) = i b && (go l || go r)
   i = intf ray
   
-- | Splits the given @Primitive@ list along the specified @Dimension@
--   in two lists
splitMidpoint :: [AnyPrim] -> Dimension -> ([AnyPrim], [AnyPrim])
splitMidpoint ps dim = ([l | l <- ps, toLeft l], [r | r <- ps, not $ toLeft r]) where
   toLeft p = component (centroid $ worldBounds p) dim < pMid
   pMid = 0.5 * (component (aabbMin cb) dim + component (aabbMax cb) dim)
   cb = centroidBounds ps

intAABB :: AABB -> Ray -> Vector -> (Int, Int, Int) -> Bool
{-# INLINE intAABB #-}
intAABB b (Ray (MkVector rox roy roz) _ rmin rmax) (MkVector idx idy idz) (inx, iny, inz)
   | tmin > tymax || tymin > tmax = False
   | tmin' > tzmax || tzmin > tmax' = False
   | otherwise = tmin'' < rmax && tmax'' > rmin
   where
         e axis = if axis == 0 then aabbMin b else aabbMax b
         tmin = ((getX (e      inx)  - rox)) * idx
         tmax = ((getX (e (1 - inx)) - rox)) * idx
         tymin = ((getY (e      iny)  - roy)) * idy
         tymax = ((getY (e (1 - iny)) - roy)) * idy
         
         tmin' = if tymin > tmin then tymin else tmin
         tmax' = if tymax < tmax then tymax else tmax
         tzmin = ((getZ (e       inz) - roz)) * idz
         tzmax = ((getZ (e (1 - inz)) - roz)) * idz
         tmin'' = if tzmin > tmin' then tzmin else tmin'
         tmax'' = if tzmax < tmax' then tzmax else tmax'
         
-- | Finds the preferred split axis for a list of primitives. This
--   is where the AABBs centroid's bounds have the maximum extent
splitAxis :: [AnyPrim] -> Dimension
splitAxis = maximumExtent . centroidBounds

-- | Finds the AABB of the specified @Primitive@'s centroids
centroidBounds :: [AnyPrim] -> AABB
centroidBounds ps = foldl extendAABBP emptyAABB $ centroids ps

-- | Finds the centroids of a list of primitives
centroids :: [AnyPrim] -> [Point]
centroids = map (centroid . worldBounds)
