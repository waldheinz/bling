
module Graphics.Bling.Primitive.KdTree (
   KdTree, mkKdTree
   ) where

import Debug.Trace

import Data.List (foldl')   
import qualified Data.Vector as V

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Primitive

data KdTree = KdTree
   { treeBounds :: AABB
   , treeRoot :: KdTreeNode
   } deriving (Show)
   
data KdTreeNode
   = Interior
      { leftChild :: KdTreeNode
      , rightChild :: KdTreeNode
      , splitPos :: ! Flt
      , splitAxis :: ! Dimension
      }
   | Leaf
      { leafPrims :: ! (V.Vector AnyPrim)
      }

instance Show KdTreeNode where
   show (Interior l r t a) = "I t=" ++ show t ++ ", a=" ++ show a ++ "("
      ++ show l ++ ") (" ++ show r ++ ")"
   show (Leaf ps) = "L pc=" ++ show (V.length ps)
      
--
-- Creation
--

data Edge = Edge
   { edgePrim  :: ! BP
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
   bps = V.map (\p -> BP p (worldBounds p)) (V.fromList ps)
   bounds = V.foldl' extendAABB emptyAABB $ V.map bpBounds bps
   maxDepth = 1 -- round (8 + 1.3 * log (fromIntegral $ V.length bps))
   
buildTree :: AABB -> V.Vector BP -> Int -> KdTreeNode
buildTree bounds bps depth
   | trace ("build " ++ show bounds ++ ", pc=" ++ show (V.length bps)) False = undefined
   | depth == 0 || V.length bps <= 1 = Leaf $ V.map bpPrim bps
   | otherwise = trace ("int t=" ++ show t ++ ", axis=" ++ show axis) $ Interior left right t axis
   where
      left = buildTree lb lp (depth - 1)
      right = buildTree rb rp (depth - 1)
      (lp, rp) = partition edges t
      (lb, rb) = splitAABB t axis bounds
      t = bestSplit bounds axis $ allSplits (V.toList edges) edgeCount
      axis = maximumExtent bounds
      edgeCount = 2 * V.length bps
      edges = V.generate (2 * V.length bps) ef where -- TODO: sort edges!
         ef i = let bp = bps V.! (i `div` 2)
                    start = (i `div` 2 == 0)
                    (AABB pmin pmax) = bpBounds bp
                    et = if start then pmin .! axis else pmax .! axis
                    in Edge bp et start

partition :: V.Vector Edge -> Flt -> (V.Vector BP, V.Vector BP)
partition es t = (\(ls, rs) -> (V.fromList ls, V.fromList rs)) $ V.foldl go  ([], []) es where
   go (ls, rs) (Edge p et start)
      | et <= t && start = (p : ls, rs)
      | et > t && (not start) = (ls, p:rs)
      | otherwise = (ls, rs)
      
-- | a split is (t, # prims left, # prims right)
type Split = (Flt, Int, Int)

bestSplit :: AABB -> Dimension -> [Split] -> Flt
bestSplit bounds axis ss = trace ("bs " ++ show (length ss)) $ snd $ go ss (infinity, undefined) where
   go [] x = x
   go ((t, nl, nr):xs) s@(c, _)
      | trace ("c'=" ++ show c') $ c' < c = go xs (c', t)
      | otherwise = s
      where
         c' = cost bounds axis t nl nr
         
allSplits :: [Edge] -> Int -> [Split]
allSplits e ec = trace ("as " ++ show (length e)) $ allSplits' e 0 ec where
   allSplits' ((Edge _ t True):es') l r = (t, l, r):(allSplits' es' (l+1) r)
   allSplits' ((Edge _ t False):es') l r = (t, l, r-1):allSplits' es' l (r-1)
   allSplits' _ _ _ = []

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
   