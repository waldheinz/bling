
module Graphics.Bling.Montecarlo (
   
   -- * 1D and 2D Distributions

   Dist1D, mkDist1D, sampleDiscrete1D, sampleContinuous1D,
   Dist2D, mkDist2D, sampleContinuous2D, pdfDist2D,
   
   -- * MIS Combination Strategies
   
   MisHeuristic, powerHeuristic, balanceHeuristic,
   
   -- * Misc Sampling Functions
   
   uniformSampleCone, uniformConePdf, cosineSampleHemisphere,
   concentricSampleDisk, concentricSampleDisk', uniformSampleSphere,
   uniformSampleHemisphere, uniformSampleTriangle
   ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector as BV

import Graphics.Bling.Math
import Graphics.Bling.Random

--------------------------------------------------------------------------------
-- 1D - distribution
--------------------------------------------------------------------------------

data Dist1D = MkDist1D {
   distFunc :: V.Vector Flt,
   _cdf     :: V.Vector Flt,
   funcInt  :: Flt
   }

integ :: (Num a) => [a] -> (a, [a])
integ vs = (sm, Prelude.tail int) where
   (sm, int, _) = integ' (0, [], vs)
   integ' (s, is, []) = (s, is Prelude.++ [s], [])
   integ' (s, is, f:fs) = integ' (s + f, is Prelude.++ [s], fs)

mkDist1D :: [Flt] -> Dist1D
mkDist1D ls = MkDist1D f (V.fromList c) fi where
   f = V.fromList ls
   (fi, i) = integ ls
   cnt = V.length f
   c = 0 : if fi == 0
      then Prelude.take cnt (Prelude.map (/fromIntegral cnt) [1..])
      else Prelude.map (/fi) i

count :: Dist1D -> Int
count (MkDist1D f _ _) = V.length f

upperBound :: V.Vector Flt -> Flt -> Int
upperBound v u = max 0 $ maybe (V.length v - 1) (\i -> i - 1) $ V.findIndex (>= u) v

sampleDiscrete1D
   :: Dist1D      -- ^ the distribution to sample
   -> Flt         -- ^ the variate for sampling, must be in [0..1)
   -> (Int, Flt)  -- ^ (sampled offset, pdf)
sampleDiscrete1D d@(MkDist1D f c fi) u
   | u < 0 = error "sampleDiscrete1D : u < 0"
   | u >= 1 = error "sampleDiscrete1D : u >= 1"
   | otherwise = (offset, pdf) where
      offset = upperBound c u
      pdf = V.unsafeIndex f offset / (fi * fromIntegral (count d))

sampleContinuous1D :: Dist1D -> Flt -> (Flt, Flt, Int)
sampleContinuous1D (MkDist1D func cdf fi) u = (x, pdf, offset) where
   offset = upperBound cdf u
   pdf = if fi == 0 then 0 else (func V.! offset) / fi
   rel = if cdf V.! (offset + 1) == 0
            then 0
            else (cdf V.! offset) / (cdf V.! (offset + 1))
   du = u - rel - (cdf V.! offset)
   x = (fromIntegral offset + du) / fromIntegral (V.length func)

-- | a 2D distribution
data Dist2D = MkDist2D
                (BV.Vector Dist1D) -- conditional
                Dist1D -- marginal
                  
-- | creates a 2D distribution
mkDist2D
   :: PixelSize         -- ^ area to cover
   -> (PixelPos -> Flt) -- ^ evaluation function
   -> Dist2D
mkDist2D (nu, nv) fun = MkDist2D conditional marginal where
   conditional = BV.generate nv $ \v ->
      mkDist1D [fun (u, v) | u <- [0..nu-1]]
   marginal = mkDist1D' $ GV.map funcInt conditional
   mkDist1D' x = mkDist1D $ GV.toList x

sampleContinuous2D :: Dist2D -> Rand2D -> (CartesianCoords, Flt)
sampleContinuous2D (MkDist2D cond marg) (u0, u1) = (Cartesian (u, v), pdf0 * pdf1) where
   (v, pdf1, imarg) = sampleContinuous1D marg u1
   (u, pdf0, _) = sampleContinuous1D (cond BV.! imarg) u0

pdfDist2D :: Dist2D -> CartesianCoords -> Flt
pdfDist2D (MkDist2D cond marg) (Cartesian (u, v))
   | funcInt marg * funcInt (cond BV.! iv) == 0 = 0
   | otherwise = (distFunc (cond BV.! iv) V.! iu * distFunc marg V.! iv) /
                 (funcInt (cond BV.! iv) * funcInt marg)
   where
      iu' = floor $ u * (fromIntegral $ count $ cond BV.! 0)
      iu = max 0 $ min (count (cond BV.! 0) - 1) iu'
      iv' = floor $ v * (fromIntegral $ count marg)
      iv = max 0 $ min (count marg - 1) iv'

--------------------------------------------------------------------------------
-- MIS combination strategies
--------------------------------------------------------------------------------

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
      x * vpromote (cos phi * sinTheta) +
      y * vpromote (sin phi * sinTheta) +
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
{-# INLINE uniformSampleHemisphere #-}
uniformSampleHemisphere d u
   | d `dot` rd < 0 = -rd
   | otherwise = rd
   where
      rd = uniformSampleSphere u

uniformSampleTriangle
   :: Rand2D
   -> (Flt, Flt) -- ^ first and second barycentric
{-# INLINE uniformSampleTriangle #-}
uniformSampleTriangle (u1, u2) = (1 - su1, u2 * su1) where
   su1 = sqrt u1
   