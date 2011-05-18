
module Graphics.Bling.Primitive.Bvh 
   ( Bvh, mkBvh, ppBvh ) 
   where

import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Text.PrettyPrint

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Primitive

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
      !(V.Vector AnyPrim)
      {-# UNPACK #-} !AABB

--
-- pretty printing BVH stats
--

ppBvh :: TreeBvh -> Doc
ppBvh t = vcat [
   text "primitive count" <+> int p,
   text "maximum depth" <+> int md,
   text "maximum leaf prims" <+> int (maxPrims t),
   text "number of leaves" <+> int l,
   text "avg. depth" <+> float (fromIntegral sd / fromIntegral l),
   text "avg. prims per leaf" <+> float (fromIntegral p / fromIntegral l)
   ] where
      (md, sd) = maxDepth t
      l = leafCount t
      p = primCount t

primCount :: TreeBvh -> Int
primCount (Leaf ps _) = V.length ps
primCount (Node _ l r _) = primCount l + primCount r

maxPrims :: TreeBvh -> Int
maxPrims (Leaf ps _) = V.length ps
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
   leafCount' (Node _ l r _) = leafCount' l + leafCount' r

instance Primitive TreeBvh where
   intersects = bvhIntersects
   intersect = bvhIntersect
   worldBounds (Node _ _ _ b) = b
   worldBounds (Leaf _ b) = b
   flatten _ = error "Unimplemented BVH.flatten"
   
mkBvh :: [AnyPrim] -> TreeBvh
mkBvh [] = Leaf V.empty emptyAABB
mkBvh [p] = Leaf (V.singleton p) $ worldBounds p
mkBvh ps
   | null left = Leaf (V.fromList right) allBounds
   | null right = Leaf (V.fromList left) allBounds
   | otherwise = Node dim (mkBvh left) (mkBvh right) allBounds where
   (left, right) = splitMidpoint ps dim
   dim = splitAxis ps
   allBounds = foldl extendAABB emptyAABB $ map worldBounds ps

bvhIntersect :: TreeBvh -> Ray -> Maybe Intersection
bvhIntersect bvh ray = go bvh ray where
   i = intf ray
   go (Leaf p b) r = if i b (rayMax r) then nearest p r else Nothing
   go (Node d lt rt b) r@(Ray _ rd _ tmax) = if i b tmax then near fi oi else Nothing where
      (fc, oc) = if rd .! d >= 0 then (lt, rt) else (rt, lt)
      fi = go fc r  -- first intersection
      oi = go oc r' -- other intersection
      r' = maybe r (\i' -> r {rayMax = intDist i'}) fi -- ray clipped against fi
      
near :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
near Nothing i = i
near i Nothing = i
near mi1 mi2 = Just $ near' (fromJust mi1) (fromJust mi2) where
   near' i1@(Intersection d1 _ _ _) i2@(Intersection d2 _ _ _)
      | d1 < d2 = i1
      | otherwise = i2

-- | creates an itersection function which can be used to check multiple
--   AABBs for intersection against a single @Ray@
intf :: Ray -> AABB -> Float -> Bool
{-# INLINE intf #-}
intf ray@(Ray _ (Vector dx dy dz) _ _) = \b m -> intAABB b ray {rayMax = m} invD negD where
   invD = mkV (1 / dx, 1 / dy, 1 / dz)
   negD = (isNeg dx, isNeg dy, isNeg dz)
   isNeg x = if x < 0 then 1 else 0
   
bvhIntersects :: TreeBvh -> Ray -> Bool
bvhIntersects bvh ray = go bvh where
   go (Leaf p b) = i b && V.any (`intersects` ray) p
   go (Node _ l r b) = i b && (go l || go r)
   i b = intf ray b (rayMax ray)
   
-- | Splits the given @Primitive@ list along the specified @Dimension@
--   in two lists
splitMidpoint :: [AnyPrim] -> Dimension -> ([AnyPrim], [AnyPrim])
splitMidpoint ps dim = ([l | l <- ps, toLeft l], [r | r <- ps, not $ toLeft r]) where
   toLeft p = (centroid $ worldBounds p) .! dim < pMid
   pMid = 0.5 * ((aabbMin cb) .! dim + (aabbMax cb) .! dim)
   cb = centroidBounds ps

intAABB :: AABB -> Ray -> Vector -> (Int, Int, Int) -> Bool
{-# INLINE intAABB #-}
intAABB b (Ray (Vector rox roy roz) _ rmin rmax) (Vector idx idy idz) (inx, iny, inz)
   | tmin > tymax || tymin > tmax = False
   | tmin' > tzmax || tzmin > tmax' = False
   | otherwise = tmin'' < rmax && tmax'' > rmin
   where
         e axis = if axis == 0 then aabbMin b else aabbMax b
         tmin =  (vx (e      inx)  - rox) * idx
         tmax =  (vx (e (1 - inx)) - rox) * idx
         tymin = (vy (e      iny)  - roy) * idy
         tymax = (vy (e (1 - iny)) - roy) * idy
         
         tmin' = if tymin > tmin then tymin else tmin
         tmax' = if tymax < tmax then tymax else tmax
         tzmin = (vz (e       inz) - roz) * idz
         tzmax = (vz (e (1 - inz)) - roz) * idz
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
