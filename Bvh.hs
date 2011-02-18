
module Bvh where

import Math
import Primitive

data Bvh
   = Node {
      leftChild :: Bvh,
      rightChild :: Bvh,
      bounds :: AABB
      }
   | Leaf {
      prims :: [Primitive],
      bounds :: AABB
      }

splitAxis :: [Primitive] -> Dimension
splitAxis ps = maximumExtent $ foldl extendAABBP emptyAABB $map (centroid . primBounds) ps