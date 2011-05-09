
module Graphics.Bling.Primitive.KdTree (
   KdTree, mkKdTree
   ) where

import Data.List (foldl')   
import Data.Vector as V

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Primitive

data KdTree = KdTree
   { treeBounds :: AABB
   , treeRoot :: KdTreeNode
   }
   
data KdTreeNode
   = Interior
      { leftChild :: KdTree
      , rightChild :: KdTree
      , splitPos :: ! Flt
      , splitAxis :: ! Dimension
      }
   | Leaf
      { leafPrims :: ! (V.Vector AnyPrim)
      }
      
--
-- Creation
--

data Edge = Edge
   { edgePrim  :: ! AnyPrim
   , edgeT     :: ! Flt
   , edgeStart :: ! Bool
   }

data BP = BP
   { bpPrim    :: ! AnyPrim
   , bpBounds  :: ! AABB
   }

mkKdTree :: [AnyPrim] -> KdTree
mkKdTree ps = KdTree bounds root where
   root = buildTree bounds bps maxDepth
   bps = V.map (\p -> BP p (worldBounds p)) (fromList ps)
   bounds = V.foldl' extendAABB emptyAABB $ V.map bpBounds bps
   maxDepth = round (8 + 1.3 * log (fromIntegral $ V.length bps))
   
buildTree :: AABB -> V.Vector BP -> Int -> KdTreeNode
buildTree bounds ps depth
   | depth == 0 || V.length ps == 1 = Leaf $ V.map bpPrim ps
   | otherwise = Interior lc rc t axis
   where
      (lc, rc) = undefined
      t = undefined
      axis = maximumExtent bounds

-- | the SAH cost function
cost
   :: AABB -- ^ the bounds of the region to be split
   -> Dimension -- ^ the dimension to split along
   -> Flt -- ^ the split position
   -> Int -- ^ the number of prims to the left of the split
   -> Int -- ^ the number of prims to the right of the split
   -> Flt -- ^ the resulting split cost according to the SAH
cost b@(AABB pmin pmax) axis t nl nr = cT + cI * (1 - eb) * pI where
   pI = (pl * fromIntegral nl + pr * fromIntegral nr)
   cT = 1 -- cost for traversal
   cI = 80  -- cost for primitive intersection
   eb = if nl == 0 || nr == 0 then 0.5 else 0
   (pl, pr) = (sal * invTotSa, sar * invTotSa)
   invTotSa = 1 / surfaceArea b
   d = pmax - pmin
   (oa0, oa1) = ((axis + 1) `mod` 3 , (axis + 2) `mod` 3)
   sal = 2 * (d .! oa0 * d .! oa1 + (t - pmin .! axis) * d .! oa0 + d .! oa1)
   sar = 2 * (d .! oa0 * d .! oa1 + (pmax .! axis - t) * d .! oa0 + d .! oa1)
   
instance Primitive KdTree where
   flatten t = [MkAnyPrim t]
   
   worldBounds (KdTree b _) = b
   