
module Graphics.Bling.Integrator.BidirPath(
   BidirPath, mkBidirPathIntegrator
   ) where

import Data.BitSet
import Control.Monad (liftM)
import qualified Data.Vector.Generic as V

import Graphics.Bling.Integrator
import Graphics.Bling.Light
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Reflection
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data BidirPath = BidirPath

mkBidirPathIntegrator :: BidirPath
mkBidirPathIntegrator = BidirPath

instance SurfaceIntegrator BidirPath where
   

-- | a path vertex
data Vertex = Vertex {
   vint :: Intersection,
   vbsdf :: Bsdf
   }

evalPath :: [Vertex] -> Rand WeightedSpectrum
evalPath vs = do
   return (1, fromRGB (1, 1, 1))

tracePath :: Scene -> Ray -> Rand [Vertex]
tracePath s r = evalInt s (-(rayDir r)) (s `intersect` r)
   
evalInt :: Scene -> Vector -> Maybe Intersection -> Rand [Vertex]
evalInt _ _ Nothing = return []
evalInt s wo (Just int) = do
   bsdfDirU <- rnd2D
   bsdfCompU <- rnd
   let (BsdfSample smpType spdf f wi) = sampleBsdf bsdf wo bsdfCompU bsdfDirU
   (liftM . (:)) (Vertex int bsdf) $ tracePath s (Ray p wi epsilon infinity) where
      bsdf = intBsdf int
      dg = intGeometry int
      p = dgP dg

directLight :: Scene -> Ray -> Spectrum
directLight s ray = V.foldl (+) black (V.map (`le` ray) (sceneLights s))

