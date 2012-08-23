
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Bling.KdTree (

   
   
   ) where

import Debug.Trace

import Control.Monad.Primitive
import Data.Function (on)
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic.Mutable as MV

class Dimensional a where
   dims     :: a -> Int
   dimOrd   :: Int -> a -> a -> Ordering

instance (Ord a, Ord b) => Dimensional (a, b) where
   dims _ = 2
   
   dimOrd 0 = compare `on` fst
   dimOrd 1 = compare `on` snd
   dimOrd x = error ("dimension " ++ (show x) ++ " out of bounds")

data KdTree a
   = Node a (KdTree a) (KdTree a)
   | Empty
   deriving (Show)

{-
   
   runST $ DV.thaw (DV.fromList [(2,3),(5,4),(9,6),(4,7),(8,1),(7,2)]) >>= \v -> (mkKdTree 0 v)

Node (7,2)
   (Node (5,4)
      (Node (2,3) Empty Empty)
      (Node (4,7) Empty Empty))
   (Node (9,6) (Node (8,1) Empty Empty) Empty)
-}

mkKdTree :: forall a m v. (PrimMonad m, MV.MVector v a, Dimensional a, Show a) => Int -> v (PrimState m) a -> m (KdTree a)
mkKdTree depth v
   | MV.null v = return Empty
   | MV.length v == 1 = MV.unsafeRead v 0 >>= \e -> return $ Node e Empty Empty
   | otherwise = do
      let
         median = MV.length v `div` 2
         axis = depth `rem` dims (undefined :: a)
   
      I.partialSortBy (dimOrd axis) v median
      
      pivot <- MV.read v median
      left <- traceShow (depth, axis, median, pivot) $ mkKdTree (depth + 1) $ MV.take median v
      right <- mkKdTree (depth + 1) $ MV.drop (median+1) v
      
      return $ Node pivot left right
      
