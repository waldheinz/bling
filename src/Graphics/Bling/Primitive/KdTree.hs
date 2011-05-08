
module Graphics.Bling.Primitive.KdTree (
   KdTree, mkKdTree
   ) where

import Data.List (foldl')   

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
      , splitPos :: Flt
      }
   | Leaf
      { leafPrims :: [AnyPrim]
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
   bps = map (\p -> BP p (worldBounds p)) ps
   bounds = foldl' extendAABB emptyAABB $ map bpBounds bps
   maxDepth = round (8 + 1.3 * log (fromIntegral $ length ps))
      
buildTree :: AABB -> [BP] -> Int -> KdTreeNode
buildTree bounds ps depth
   | depth == 0 = Leaf $ map bpPrim ps
   | otherwise = Interior lc rc split
   where
      (lc, rc) = undefined
      split = undefined
   
instance Primitive KdTree where
   
   
