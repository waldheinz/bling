{-# LANGUAGE ExistentialQuantification #-}

module Bvh 
   ( Bvh ) 
   where

import Data.Maybe (isJust)

import AABB
import Math
import Primitive

data Bvh
   = Node {
      leftChild :: Bvh,
      rightChild :: Bvh,
      bounds :: AABB
      }
   | Leaf {
      prim :: AnyPrim,
      bounds :: AABB
      }

instance Prim Bvh where
   primIntersects bvh r = bvhIntersects bvh r
   primIntersect bvh r = bvhIntersect bvh r
   primWorldBounds (Node _ _ b) = b
   primWorldBounds (Leaf _ b) = b

-- mkBvh :: [AnyPrim] -> Bvh
-- mkBvh [p] = GeometricB undefined (primIntersects tree) undefined Nothing (primBounds p) where
--   tree = Leaf p (primBounds p)

bvhIntersect :: Bvh -> Ray -> Maybe Intersection
bvhIntersect _ _ = Nothing

bvhIntersects :: Bvh -> Ray -> Bool
bvhIntersects (Leaf p b) r = (isJust $ intersectAABB b r) && (primIntersects p r)
bvhIntersects (Node l r b) ray = (isJust $ intersectAABB b ray) &&
   ((bvhIntersects l ray) || (bvhIntersects r ray))
   
-- | Splits the given @Primitive@ list along the specified @Dimension@
--   in two lists
splitMidpoint :: [Primitive] -> Dimension -> ([Primitive], [Primitive])
splitMidpoint ps dim = ([l | l <- ps, toLeft l], [r | r <- ps, not $ toLeft r]) where
   toLeft p = component (centroid $ primBounds p) dim < pMid
   pMid = 0.5 * ((component (aabbMin cb) dim) + (component (aabbMax cb) dim))
   cb = centroidBounds ps
   
-- | Finds the preferred split axis for a list of primitives. This
--   is where the AABBs centroid's bounds have the maximum extent
splitAxis :: [Primitive] -> Dimension
splitAxis = maximumExtent . centroidBounds

-- | Finds the AABB of the specified @Primitive@'s centroids
centroidBounds :: [Primitive] -> AABB
centroidBounds ps = foldl extendAABBP emptyAABB $ centroids ps

-- | Finds the centroids of a list of primitives
centroids :: [Primitive] -> [Point]
centroids = map (centroid . primBounds)
