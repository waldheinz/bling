
module Graphics.Bling.Primitive.KdTree (
   
   -- * Kd - Trees
   KdTree, mkKdTree, kdTreePrimitive,

   -- * Debugging 

   ppKdTree, dbgTraverse, TraversalStats(..)
   ) where

import Control.Monad (forM_)
import Control.Monad.ST
import Control.Parallel
import Data.Maybe (fromMaybe)
import Text.PrettyPrint   
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Mutable as MV

import Graphics.Bling.AABB
import Graphics.Bling.Math
import Graphics.Bling.Primitive

data KdTree = KdTree {-# UNPACK #-} ! AABB ! KdTreeNode deriving (Show)

data KdTreeNode
   = Interior !KdTreeNode !KdTreeNode {-# UNPACK #-} !Float {-# UNPACK #-} !Dimension
   | Leaf {-# UNPACK #-} !(V.Vector Primitive)
   
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
   text "number of empty leaves" <+> int el,
   text "avg. depth" <+> float (fromIntegral sd / fromIntegral l),
   text "avg. prims per leaf" <+> float (fromIntegral p / fromIntegral l)
   ] where
      (md, sd, el) = maxDepth t
      l = leafCount t
      p = primCount t

primCount :: KdTreeNode -> Int
primCount (Leaf ps) = V.length ps
primCount (Interior l r _ _) = primCount l + primCount r

maxPrims :: KdTreeNode -> Int
maxPrims (Leaf ps) = V.length ps
maxPrims (Interior l r _ _) = max (maxPrims l) (maxPrims r)

maxDepth :: KdTreeNode -> (Int, Int, Int)
maxDepth t = maxDepth' t (0, 0, 0) where
   maxDepth' (Leaf ps) (m, s, el) = (m + 1, s + 1, el + if V.null ps then 1 else 0)
   maxDepth' (Interior l r _ _) (m, s, el) = (max ml mr, sl + sr, el + ell + elr) where
      (ml, sl, ell) = maxDepth' l (m + 1, s + 1, el)
      (mr, sr, elr) = maxDepth' r (m + 1, s + 1, el)

leafCount :: KdTreeNode -> Int
leafCount (Leaf _) = 1
leafCount (Interior l r _ _) = leafCount l + leafCount r

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | cost for traversal
cT :: Float
cT = 1

-- | cost for primitive intersection
cI :: Float
cI = 80

data BP = BP
   { bpPrim    :: ! Primitive
   , bpBounds  :: {-# UNPACK #-} ! AABB
   }

data Edge = Edge {-# UNPACK #-} !BP {-# UNPACK #-} !Float !Bool

instance Eq Edge where
   (Edge _ t1 s1) == (Edge _ t2 s2) = t1 == t2 && s1 == s2

instance Ord Edge where
   (Edge _ t1 s1) <= (Edge _ t2 s2)
      | t1 == t2 = s1 && not s2
      | otherwise = t1 <= t2

instance Show Edge where
   show (Edge _ t s)
      | s = "S " ++ show t
      | otherwise = "E " ++ show t

mkKdTree :: [Primitive] -> KdTree
mkKdTree ps = {-# SCC "mkKdTree" #-} KdTree bounds root where
   root = buildTree bounds bps md
   bps = V.map (\p -> BP p (worldBounds p)) (V.fromList ps)
   bounds = V.foldl' extendAABB emptyAABB $ V.map bpBounds bps
   md = round (8 + 3 * log (fromIntegral $ V.length bps :: Float))
   
buildTree :: AABB -> V.Vector BP -> Int -> KdTreeNode
buildTree bounds bps depth
   | depth == 0 || V.length bps <= 1 = {-# SCC "buildTree.leaf" #-} leaf
   | otherwise = fromMaybe leaf $ trySplit bounds bps depth
   where
      leaf = Leaf $ V.map bpPrim bps
      
trySplit :: AABB -> V.Vector BP -> Int -> Maybe KdTreeNode
trySplit bounds bps depth = {-# SCC "trySplit" #-} go Nothing axii where
   axii = [a `rem` 3 | a <- take 3 [(maximumExtent bounds)..]]
   oldCost = cI * fromIntegral (V.length bps)
   
   go :: Maybe KdTreeNode -> [Int] -> Maybe KdTreeNode
   go o [] = o
   go o (axis:axs)
      | null fs = go o axs
      | c < oldCost = par left (seq right (Just $ Interior left right t axis))
      | otherwise = go o axs
      where
         (c, (i, t, _, _)) = bestSplit bounds axis fs
         fs = filterSplits axis bounds $ allSplits es
         es = edges bps axis
         left = buildTree lb lp (depth - 1)
         right = buildTree rb rp (depth - 1)
         (lp, rp) = partition es i
         (lb, rb) = splitAABB t axis bounds

filterSplits :: Dimension -> AABB -> [Split] -> [Split]
filterSplits axis b = filter (\(_, t, _, _) -> (t > tmin) && (t < tmax)) where
   tmin = aabbMin b .! axis
   tmax = aabbMax b .! axis

partition :: V.Vector Edge -> Int -> (V.Vector BP, V.Vector BP)
partition es i = {-# SCC "partition" #-} (lp, rp) where
   lp = V.map (\(Edge p _ _) -> p) $ V.filter (\(Edge _ _ st) -> st) le
   rp = V.map (\(Edge p _ _) -> p) $ V.filter (\(Edge _ _ st) -> not st) re
   (le, re) = (V.take i es, V.drop (i+1) es) 

-- | a split is (offset into es, t, # prims left, # prims right)
type Split = (Int, Float, Int, Int)

bestSplit :: AABB -> Dimension -> [Split] -> (Float, Split)
bestSplit bounds axis fs = {-# SCC "bestSplit" #-} go fs (infinity, undefined) where
   go [] s = s
   go (s@(_, t, nl, nr):xs) x@(c, _)
      | c' < c = go xs (c', s)
      | otherwise = go xs x where
         c' = cost bounds axis t nl nr

allSplits :: V.Vector Edge -> [Split]
allSplits es = {-# SCC "allSplits" #-} go 0 (V.toList es) 0 (V.length es `div` 2) where
   go i (Edge _ t False : es') l r = (i, t, l, r-1):go (i+1) es' l (r-1)
   go i (Edge _ t True  : es') l r = (i, t, l, r  ):go (i+1) es' (l+1) r
   go _ [] _ _ = []

edges :: V.Vector BP -> Dimension -> V.Vector Edge
edges bps axis = {-# SCC "edges" #-} runST $ do
   v <- MV.new (2 * V.length bps)
   
   forM_ [0..V.length bps-1] $ \idx -> do
      let
         bp = bps V.! idx
         (AABB pmin pmax) = bpBounds bp
         e1 = Edge bp (pmin .! axis) True
         e2 = Edge bp (pmax .! axis) False
         
      MV.write v (2 * idx + 0) e1
      MV.write v (2 * idx + 1) e2
   
   {-# SCC "edges.sort" #-} VA.sort v
   V.freeze v
   
-- | the SAH cost function
cost
   :: AABB        -- ^ the bounds of the region to be split
   -> Dimension   -- ^ the dimension to split along
   -> Float       -- ^ the split position
   -> Int         -- ^ the number of prims to the left of the split
   -> Int         -- ^ the number of prims to the right of the split
   -> Float       -- ^ the resulting split cost according to the SAH
cost b@(AABB pmin pmax) a0 t nl nr = {-# SCC "cost" #-} cT + cI * eb * pI where
   pI = pl * fromIntegral nl + pr * fromIntegral nr
   eb = if nl == 0 || nr == 0 then 0.5 else 1
   (pl, pr) = (sal * invTotSa, sar * invTotSa)
   invTotSa = 1 / surfaceArea b
   d = pmax - pmin
   (a1, a2) = ((a0 + 1) `mod` 3 , (a0 + 2) `mod` 3)
   (dl, dr) = (t - (pmin .! a0), (pmax .! a0) - t)
   sal = 2 * ((d .! a1) * (d .! a2) + dl * ((d .! a1) + (d .! a2)))
   sar = 2 * ((d .! a1) * (d .! a2) + dr * ((d .! a1) + (d .! a2)))

--------------------------------------------------------------------------------
-- Traversal
--------------------------------------------------------------------------------

-- | traversal function for @Primitive.intersects@
traverse' :: Ray -> Vector -> KdTreeNode -> (Float, Float) -> Bool
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
traverse :: (Ray, Maybe Intersection) -> Vector -> KdTreeNode -> (Float, Float) -> (Ray, Maybe Intersection)
traverse ri _ (Leaf ps) _ = nearest' ps ri
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

kdTreePrimitive :: KdTree -> Primitive
kdTreePrimitive (KdTree b t) = prim where
   prim = Primitive inter inters b flat Nothing (\_ dg -> dg)

   flat = [prim]
      
   inter r@(Ray _ d _ _) = {-# SCC "intersect" #-} intersectAABB b r >>= trav where
      trav ts = snd $ traverse (r, Nothing) invDir t ts
      invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
      
   inters r@(Ray _ d _ _) = {-# SCC "intersects" #-} maybe False tr (intersectAABB b r) where
      tr = traverse' r invDir t
      invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
      
--------------------------------------------------------------------------------
-- Kd - Tree Vision
--------------------------------------------------------------------------------

data TraversalStats = TraversalStats
   { nodesTraversed :: Int
   , intersections :: Int
   }

deeper :: TraversalStats -> TraversalStats
deeper ts = ts { nodesTraversed = nodesTraversed ts + 1 }

dbgTraverse :: KdTree -> Ray -> TraversalStats
dbgTraverse (KdTree b t) r@(Ray _ d _ _) = maybe stats t' (intersectAABB b r) where
   t' ts = let (_, _, x) = trav ts
          in x
   trav = dbgTraverse' (r, Nothing, stats) invDir t
   invDir = mkV (1 / d .! 0, 1 / d .! 1, 1 / d .! 2)
   stats = TraversalStats 0 0

-- | traversal function for @Primitive.intersect@
dbgTraverse' :: (Ray, Maybe Intersection, TraversalStats) -> Vector -> KdTreeNode -> (Float, Float) -> (Ray, Maybe Intersection, TraversalStats)
dbgTraverse' (r, mi, ts) _ (Leaf ps) _ = let (r', mi') = nearest' ps (r, mi)
                                         in (r', mi', deeper $ ts { intersections = intersections ts + V.length ps })
dbgTraverse' ri@(r, mi, ts) inv (Interior left right sp axis) mima@(tmin, tmax)
   | rayMax r < tmin = ri
   | tp > tmax || tp <= 0 = dbgTraverse' (r, mi, deeper ts) inv fc mima
   | tp < tmin = dbgTraverse' (r, mi, deeper ts) inv sc mima
   | otherwise = dbgTraverse' (dbgTraverse' ri inv fc (tmin, tp)) inv sc (tp, tmax)
   where
         (ro, rd) = (rayOrigin r, rayDir r)
         tp = (sp - ro .! axis) * inv .! axis
         (fc, sc) = if lf then (left, right) else (right, left)
         lf = (ro .! axis < sp) || (ro .! axis == sp && rd .! axis <= 0)
