{-# LANGUAGE ExistentialQuantification #-}

module Bvh 
   ( Bvh, mkBvh ) 
   where

import Data.Maybe (isJust, isNothing, fromJust)

import AABB
import Math
import Primitive

data Bvh
   = Node Bvh Bvh AABB
   | Leaf AnyPrim AABB

instance Prim Bvh where
   primIntersects bvh r = bvhIntersects bvh r
   primIntersect bvh r = bvhIntersect bvh r
   primWorldBounds (Node _ _ b) = b
   primWorldBounds (Leaf _ b) = b

mkBvh :: [AnyPrim] -> Bvh
mkBvh [p] = Leaf p $ primWorldBounds p
mkBvh ps = Node (mkBvh left) (mkBvh right) allBounds where
   (left, right) = splitMidpoint ps dim
   dim = splitAxis ps
   allBounds = foldl extendAABB emptyAABB $ map primWorldBounds ps

bvhIntersect :: Bvh -> Ray -> Maybe Intersection
bvhIntersect (Leaf p b) ray
   | isNothing $ intersectAABB b ray = Nothing
   | otherwise = primIntersect p ray
bvhIntersect (Node l r b) ray@(Ray _ _ _ tmax)
   | isNothing $ intersectAABB b ray = Nothing
   | otherwise = near leftInt rightInt where
      leftInt = bvhIntersect l ray
      rightInt = bvhIntersect r ray

near :: Maybe Intersection -> Maybe Intersection -> Maybe Intersection
near Nothing i = i
near i Nothing = i
near mi1 mi2 = Just $ near' (fromJust mi1) (fromJust mi2) where
   near' i1@(Intersection d1 _ _ _) i2@(Intersection d2 _ _ _)
      | d1 < d2 = i1
      | otherwise = i2

bvhIntersects :: Bvh -> Ray -> Bool
bvhIntersects (Leaf p b) r = (isJust $ intersectAABB b r) && (primIntersects p r)
bvhIntersects (Node l r b) ray = (isJust $ intersectAABB b ray) &&
   ((bvhIntersects l ray) || (bvhIntersects r ray))
   
-- | Splits the given @Primitive@ list along the specified @Dimension@
--   in two lists
splitMidpoint :: [AnyPrim] -> Dimension -> ([AnyPrim], [AnyPrim])
splitMidpoint ps dim = ([l | l <- ps, toLeft l], [r | r <- ps, not $ toLeft r]) where
   toLeft p = component (centroid $ primWorldBounds p) dim < pMid
   pMid = 0.5 * ((component (aabbMin cb) dim) + (component (aabbMax cb) dim))
   cb = centroidBounds ps
   
-- | Finds the preferred split axis for a list of primitives. This
--   is where the AABBs centroid's bounds have the maximum extent
splitAxis :: [AnyPrim] -> Dimension
splitAxis = maximumExtent . centroidBounds

-- | Finds the AABB of the specified @Primitive@'s centroids
centroidBounds :: [AnyPrim] -> AABB
centroidBounds ps = foldl extendAABBP emptyAABB $ centroids ps

-- | Finds the centroids of a list of primitives
centroids :: [AnyPrim] -> [Point]
centroids = map (centroid . primWorldBounds)
