
module Graphics.Bling.Primitive.KdTree (
   KdTree, mkKdTree, ppKdTree
   ) where

import Text.PrettyPrint

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
-- pretty printing stats
--

ppKdTree :: KdTree -> Doc
ppKdTree (KdTree _ t) = vcat [
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

primCount :: KdTreeNode -> Int
primCount (Leaf ps) = V.length ps
primCount (Interior l r _ _) = primCount l + primCount r

maxPrims :: KdTreeNode -> Int
maxPrims (Leaf ps) = V.length ps
maxPrims (Interior l r _ _) = max (maxPrims l) (maxPrims r)

maxDepth :: KdTreeNode -> (Int, Int)
maxDepth t = maxDepth' t (0, 0) where
   maxDepth' (Leaf _) (m, s) = (m + 1, s + 1)
   maxDepth' (Interior l r _ _) (m, s) = (max ml mr, sl + sr) where
      (ml, sl) = maxDepth' l (m + 1, s + 1)
      (mr, sr) = maxDepth' r (m + 1, s + 1)

leafCount :: KdTreeNode -> Int
leafCount t = leafCount' t where
   leafCount' (Leaf _) = 1
   leafCount' (Interior l r _ _) = leafCount' l + leafCount' r


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
   root = buildTree bounds bps md
   bps = V.map (\p -> BP p (worldBounds p)) (V.fromList ps)
   bounds = V.foldl' extendAABB emptyAABB $ V.map bpBounds bps
   md = round (8 + 1.3 * log (fromIntegral $ V.length bps :: Flt))
   
buildTree :: AABB -> V.Vector BP -> Int -> KdTreeNode
buildTree bounds bps depth
--   | trace ("build " ++ show bounds ++ ", pc=" ++ show (V.length bps)) False = undefined
   | depth == 0 || V.length bps <= 1 = leaf
   | otherwise = maybe leaf split mbs
   where
      leaf = Leaf $ V.map bpPrim bps
      split (i, t, _, _) = Interior left right t axis where
         left = buildTree lb lp (depth - 1)
         right = buildTree rb rp (depth - 1)
         (lp, rp) = partition es i
         (lb, rb) = splitAABB t axis bounds
      mbs = bestSplit bounds axis splits
      splits = allSplits es
      axis = maximumExtent bounds
      es = edges bps axis

partition :: V.Vector Edge -> Int -> (V.Vector BP, V.Vector BP)
partition es i = (lp, rp) where
   lp = V.map (\(Edge p _ _) -> p) $ V.filter (\(Edge _ _ st) -> st) le
   rp = V.map (\(Edge p _ _) -> p) $ V.filter (\(Edge _ _ st) -> not st) re
   (le, re) = (V.take (i-1) es, V.drop i es) 

-- | a split is (index to es, t, # prims left, # prims right)
type Split = (Int, Flt, Int, Int)

bestSplit :: AABB -> Dimension -> [Split] -> Maybe Split
bestSplit bounds axis ss
   | null fs = Nothing
   | otherwise = Just $ go fs (infinity, undefined) where
   tmin = aabbMin bounds .! axis
   tmax = aabbMax bounds .! axis
   fs = filter (\(_, t, _, _) -> (t > tmin) && (t < tmax)) ss
   
   go [] (_, s) = s
   go (s@(_, t, nl, nr):xs) x@(c, _)
      | c' < c = go xs (c', s)
      | otherwise = go xs x
      where
         c' = cost bounds axis t nl nr

allSplits :: V.Vector Edge -> [Split]
allSplits es = go 0 (V.toList es) 0 (V.length es) where
   go i ((Edge _ t False):es') l r = (i, t, l, r-1):go (i+1) es' l (r-1)
   go i ((Edge _ t True ):es') l r = (i, t, l, r  ):go (i+1) es' (l+1) r
   go _ [] _ _ = []

edges :: V.Vector BP -> Dimension -> V.Vector Edge
edges bps axis = V.fromList $ sort $ concatMap te $ V.toList bps where
   te bp = [Edge bp (pmin .! axis) True, Edge bp (pmax .! axis) False] where
      (AABB pmin pmax) = bpBounds bp
   
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

-- | traversal function for @Primitive.intersects@
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

-- | traversal function for @Primitive.intersect@
traverse :: (Ray, Maybe Intersection) -> Vector -> KdTreeNode -> (Flt, Flt) -> (Ray, Maybe Intersection)
traverse (r, _) _ (Leaf ps) _ca = nearest' ps r
traverse ri@(r, _) inv (Interior left right sp axis) mima@(tmin, tmax)
   | rayMax r < tmin = ri
   | tp > tmax || tp <= 0 = traverse ri inv fc mima
   | tp < tmin = traverse ri inv sc mima
   | otherwise = traverse (traverse ri inv fc (tmin, tp)) inv sc (tp, tmax)
   where
         (ro, rd) = (rayOrigin r, rayDir r)
         tp = (sp - ro .! axis) * inv .! axis
         (fc, sc) = if lf then (left, right) else (right, left)
         lf = (ro .! axis < sp) || (ro .! axis == sp && rd .! axis <= 0)

instance Primitive KdTree where
   flatten t = [MkAnyPrim t]
   
   worldBounds (KdTree b _) = b
   
   intersect (KdTree b t) r@(Ray _ d _ _) = intersectAABB b r >>= trav where
      trav ts = snd $ traverse (r, Nothing) invDir t ts
      invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
      
   intersects (KdTree b t) r@(Ray _ d _ _) = maybe False tr (intersectAABB b r) where
      tr = traverse' r invDir t
      invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
      
