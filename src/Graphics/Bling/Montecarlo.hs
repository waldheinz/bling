
module Graphics.Bling.Montecarlo (
   -- * 1D Distributions
   Dist1D, mkDist1D, sampleDiscrete,
   
   -- * MIS Combination Strategies
   
   MisHeuristic, powerHeuristic, balanceHeuristic,
   
   -- * Misc Sampling Functions
   
   uniformSampleCone, uniformConePdf, cosineSampleHemisphere,
   concentricSampleDisk, concentricSampleDisk', uniformSampleSphere,
   uniformSampleHemisphere
   ) where

import Data.Maybe (fromJust, isJust)
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.Math
import Graphics.Bling.Random

--
-- 1D - distribution
--

data (V.Unbox a) => Dist1D a = MkDist1D {
   _func :: V.Vector a,
   _cdf :: V.Vector a,
   _funcInt :: a
   }

instance (Show a, V.Unbox a) => Show (Dist1D a) where
   show (MkDist1D f _ _) = "mkDist1D " Prelude.++ show (V.toList f)

integ :: (Fractional a) => [a] -> (a, [a])
integ vs = (sm, Prelude.tail int) where
   (sm, int, _) = integ' (0, [], vs)
   integ' (s, is, []) = (s, is Prelude.++ [s], [])
   integ' (s, is, (f:fs)) = integ' (s + f, is Prelude.++ [s], fs)

mkDist1D :: (Fractional a, V.Unbox a, Enum a) => [a] -> Dist1D a
mkDist1D ls = MkDist1D f (V.fromList c) fi where
   f = V.fromList ls
   (fi, i) = integ ls
   cnt = V.length f
   c = 0.0 : if fi == 0
      then Prelude.take cnt (Prelude.map (/fromIntegral cnt) [1.0..])
      else Prelude.map (/fi) i

count :: (V.Unbox a) => Dist1D a -> Int
count (MkDist1D f _ _) = V.length f

upperBound :: (Ord a, V.Unbox a) => V.Vector a -> a -> Int
upperBound v u = if isJust i then fromJust i - 1 else V.length v - 1 where
   i = V.findIndex (>= u) v

sampleDiscrete :: (Fractional a, V.Unbox a, Ord a) => Dist1D a -> a -> (Int, a)
sampleDiscrete d@(MkDist1D f c fi) u
   | u < 0 = error "u < 0"
   | u >= 1 = error "u >= 1"
   | otherwise = (offset, pdf) where
      offset = upperBound c u
      pdf = V.unsafeIndex f offset / (fi * fromIntegral (count d))


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

--
-- misc sampling functions
--

uniformConePdf :: Flt -> Flt
{-# INLINE uniformConePdf #-}
uniformConePdf cosThetaMax = 1.0 / (twoPi * (1.0 - cosThetaMax))

uniformSampleCone :: LocalCoordinates -> Flt -> Rand2D -> Vector
{-# INLINE uniformSampleCone #-}
uniformSampleCone (LocalCoordinates x y z) cosThetaMax (u1, u2) = let
   cosTheta = lerp u1 cosThetaMax 1.0
   sinTheta = sqrt (1 - cosTheta * cosTheta)
   phi = u2 * twoPi
   in
      (
      x * vpromote ((cos phi) * sinTheta) +
      y * vpromote ((sin phi) * sinTheta) +
      z * vpromote cosTheta
      )


cosineSampleHemisphere :: Rand2D -> Vector
{-# INLINE cosineSampleHemisphere #-}
cosineSampleHemisphere u = Vector x y (sqrt (max 0 (1 - x*x - y*y))) where
   (x, y) = concentricSampleDisk u
   
concentricSampleDisk :: Rand2D -> (Flt, Flt)
{-# INLINE concentricSampleDisk #-}
concentricSampleDisk (u1, u2) = concentricSampleDisk' (sx, sy) where
   sx = u1 * 2 - 1
   sy = u2 * 2 - 1

concentricSampleDisk' :: (Flt, Flt) -> (Flt, Flt)
concentricSampleDisk' (0, 0) = (0, 0) -- handle degeneracy at origin
concentricSampleDisk' (sx, sy) = (r * cos theta, r * sin theta) where
   theta = theta' * pi / 4.0
   (r, theta')
      | sx >= (-sy) =
         if sx > sy then
            if sy > 0 then (sx, sy / sx) else (sx, 8.0 + sy / sx)
         else
            (sy, 2.0 - sx / sy)
      | sx <= sy = (-sx, 4.0 - sy / (-sx))
      | otherwise = (-sy, 6.0 + sx / (-sy))

-- | generates a random point on the unit sphere,
-- see http://mathworld.wolfram.com/SpherePointPicking.html
uniformSampleSphere :: Rand2D -> Vector
{-# INLINE uniformSampleSphere #-}
uniformSampleSphere (u1, u2) = Vector (s * cos omega) (s * sin omega) u where
   u = u1 * 2 - 1
   s = sqrt (1 - (u * u))
   omega = u2 * 2 * pi

uniformSampleHemisphere :: Vector -> Rand2D -> Vector
uniformSampleHemisphere d u
   | d `dot` rd < 0 = -rd
   | otherwise = rd
   where
      rd = uniformSampleSphere u
