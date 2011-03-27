
module Montecarlo (
   Dist1D, mkDist1D, sampleDiscrete
   ) where

import Data.Maybe (fromJust, isJust)
import Data.Vector.Unboxed as V

data (Unbox a) => Dist1D a = MkDist1D {
   func :: Vector a,
   cdf :: Vector a,
   funcInt :: a
   }

instance (Show a, Unbox a) => Show (Dist1D a) where
   show (MkDist1D f _ _) = "mkDist1D " Prelude.++ show (toList f)

integ :: (Fractional a) => [a] -> (a, [a])
integ fs = (sum, Prelude.tail int) where
   (sum, int, _) = integ' (0, [], fs)
   integ' (s, is, []) = (s, is Prelude.++ [s], [])
   integ' (s, is, (f:fs)) = integ' (s + f, is Prelude.++ [s], fs)

mkDist1D :: (Fractional a, Unbox a, Enum a) => [a] -> Dist1D a
mkDist1D ls = MkDist1D f (fromList c) fi where
   f = V.fromList ls
   (fi, i) = integ ls
   cnt = V.length f
   c = 0.0 : if fi == 0
      then Prelude.take cnt (Prelude.map (/fromIntegral cnt) [1.0..])
      else Prelude.map (/fi) i

count :: (Unbox a) => Dist1D a -> Int
count (MkDist1D f _ _) = V.length f

upperBound :: (Ord a, Unbox a) => Vector a -> a -> Int
upperBound v u = if isJust i then fromJust i - 1 else V.length v - 1 where
   i = findIndex (> u) v

sampleDiscrete :: (Fractional a, Unbox a, Ord a) => Dist1D a -> a -> (Int, a)
sampleDiscrete d@(MkDist1D f c fi) u = (offset, pdf) where
   offset = upperBound c u
   pdf = unsafeIndex f offset / (fi * fromIntegral (count d))

