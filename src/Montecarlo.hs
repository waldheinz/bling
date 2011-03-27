
module Montecarlo (
   -- * 1D Distributions
   Dist1D, mkDist1D, sampleDiscrete,
   
   -- * MIS combination strategies
   
   MisHeuristic, powerHeuristic, balanceHeuristic
   ) where

import Data.Maybe (fromJust, isJust)
import Data.Vector.Unboxed as V

--
-- 1D - distribution
--

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
   i = findIndex (>= u) v

sampleDiscrete :: (Fractional a, Unbox a, Ord a) => Dist1D a -> a -> (Int, a)
sampleDiscrete d@(MkDist1D f c fi) u
   | u < 0 = error "u < 0"
   | u >= 1 = error "u >= 1"
   | otherwise = (offset, pdf) where
      offset = upperBound c u
      pdf = unsafeIndex f offset / (fi * fromIntegral (count d))

--
-- MIS combination strategies
--

-- | a combination strategy for multiple importance sampling
type MisHeuristic = (Int, Flt) -> (Int, Flt) -> Flt

powerHeuristic :: MisHeuristic
powerHeuristic (nf, fPdf) (ng, gPdf) = (f * f) / (f * f + g * g) where
   f = fromIntegral nf * fPdf
   g = fromIntegral ng * gPdf

balanceHeuristic :: MisHeuristic
balanceHeuristic (nf, fPdf) (ng, gPdf) = (fnf * fPdf) / (fnf * fPdf + fng * gPdf) where
   fnf = fromIntegral nf
   fng = fromIntegral ng


