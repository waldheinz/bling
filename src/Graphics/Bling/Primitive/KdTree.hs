
module Graphics.Bling.Primitive.KdTree (
   KdTree, mkKdTree
   ) where

import Debug.Trace

import Data.List (sort)   
import qualified Data.Vector as V

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Primitive

data KdTree = KdTree AABB KdTreeNode deriving (Show)
   
data KdTreeNode
   = Interior KdTreeNode KdTreeNode !Flt !Dimension
   | Leaf !(V.Vector AnyPrim)
   
instance Show KdTreeNode where
   show (Interior l r t a) = "I t=" ++ show t ++ ", a=" ++ show a ++ "("
      ++ show l ++ ") (" ++ show r ++ ")"
   show (Leaf ps) = "L pc=" ++ show (V.length ps)
      
--
-- Creation
--

data Edge = Edge !BP !Flt !Bool

instance Eq Edge where
   (Edge _ t1 s1) == (Edge _ t2 s2) = t1 == t2 && s1 == s2


instance Ord Edge where
   (Edge _ t1 s1) <= (Edge _ t2 s2)
      | t1 == t2 = s1 && not s2
      | otherwise = t1 < t2

instance Show Edge where
   show (Edge _ t s)
      | s = "S " ++ show t
      | otherwise = "E " ++ show t

data BP = BP
   { bpPrim    :: ! AnyPrim
   , bpBounds  :: ! AABB
   }

mkKdTree :: [AnyPrim] -> KdTree
mkKdTree ps = KdTree bounds root where
   root = buildTree bounds bps maxDepth
   bps = V.map (\p -> BP p (worldBounds p)) (V.fromList ps)
   bounds = V.foldl' extendAABB emptyAABB $ V.map bpBounds bps
   maxDepth = round (8 + 1.3 * log (fromIntegral $ V.length bps :: Flt))
   
buildTree :: AABB -> V.Vector BP -> Int -> KdTreeNode
buildTree bounds bps depth
   | trace ("build " ++ show bounds ++ ", pc=" ++ show (V.length bps)) False = undefined
   | depth == 0 || V.length bps <= 1 = leaf
   | otherwise = maybe leaf split mt
   where
      leaf = Leaf $ V.map bpPrim bps
      split t = Interior left right t axis where
         left = buildTree lb lp (depth - 1)
         right = buildTree rb rp (depth - 1)
         (lp, rp) = partition edges' t
         (lb, rb) = splitAABB t axis bounds
      mt = trace ("splits=" ++ show splits) bestSplit bounds axis splits
      splits = trace ("edges=" ++ show edges) allSplits edges $ V.length bps
      axis = maximumExtent bounds
      edges = sort $ V.toList edges'
      edges' = V.generate (2 * V.length bps) ef where
         ef i = let bp = bps V.! (i `div` 2)
                    start = (i `mod` 2 == 0)
                    (AABB pmin pmax) = bpBounds bp
                    et = if start then pmin .! axis else pmax .! axis
                    in Edge bp et start

partition :: V.Vector Edge -> Flt -> (V.Vector BP, V.Vector BP)
partition es t = (\(ls, rs) -> (V.fromList ls, V.fromList rs)) $ V.foldl go  ([], []) es where
   go (ls, rs) (Edge p et start)
      | et < t && start = (p : ls, rs)
      | et >= t && (not start) = (ls, p:rs)
      | otherwise = (ls, rs)
      
-- | a split is (t, # prims left, # prims right)
type Split = (Flt, Int, Int)

bestSplit :: AABB -> Dimension -> [Split] -> Maybe Flt
bestSplit bounds axis ss
   | null fs = Nothing
   | otherwise = Just $ snd $ go fs (infinity, undefined) where
   tmin = aabbMin bounds .! axis
   tmax = aabbMax bounds .! axis
   fs = filter (\(t, _, _) -> (t > tmin) && (t < tmax)) ss
   
   go [] x = x
   go ((t, nl, nr):xs) s@(c, _)
      | trace ("c'=" ++ show c') $ c' < c = go xs (c', t)
      | otherwise = go xs s
      where
         c' = cost bounds axis t nl nr

allSplits :: [Edge] -> Int -> [Split]
allSplits e ec = go e 0 ec where
   go ((Edge _ t False):es') l r = (t, l, r-1):go es' l (r-1)
   go ((Edge _ t True ):es') l r = (t, l, r  ):go es' (l+1) r
   go [] _ _ = []

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
   
traverse :: Ray -> Vector -> KdTreeNode -> (Flt, Flt) -> Maybe Intersection
traverse = undefined

traverse' :: Ray -> Vector -> KdTreeNode -> (Flt, Flt) -> Bool
traverse' r _ (Leaf ps) _ca = V.any (`intersects` r) ps
traverse' r inv (Interior left right sp axis) mima@(tmin, tmax)
   | tp > tmax || tp <= 0 = traverse' r inv fc mima
   | tp < tmin = traverse' r inv sc mima
   | otherwise = traverse' r inv fc (tmin, tp) || traverse' r inv sc (tp, tmax)
   where
         (ro, rd) = (rayOrigin r, rayDir r)
         tp = (sp - ro .! axis) * inv .! axis
         (fc, sc) = if lf then (left, right) else (right, left)
         lf = (ro .! axis < sp) || (ro .! axis == sp && rd .! axis <= 0)
         
instance Primitive KdTree where
   flatten t = [MkAnyPrim t]
   
   worldBounds (KdTree b _) = b
   
   intersect (KdTree b t) r@(Ray _ d _ _) = intersectAABB b r >>= trav where
      trav = traverse r invDir t
      invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
      
   intersects (KdTree b t) r@(Ray _ d _ _) = maybe False tr (intersectAABB b r) where
      tr = traverse' r invDir t
      invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
      