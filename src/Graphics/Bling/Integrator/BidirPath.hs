
module Graphics.Bling.Integrator.BidirPath (
   BidirPath, mkBidirPathIntegrator
   ) where

import Data.BitSet
import Control.Monad (liftM)
import qualified Data.Vector.Generic as V
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Integrator
import Graphics.Bling.Light
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data BidirPath = BDP

mkBidirPathIntegrator :: BidirPath
mkBidirPathIntegrator = BDP

instance Printable BidirPath where
   prettyPrint (BDP) = PP.text "Bi-Dir Path"

instance SurfaceIntegrator BidirPath where
   li (BDP) s r = do
      e <- eyePath s r
      l <- lightPath s
      return $ connect e l

-- | a path vertex
data Vertex = Vert
   { _vbsdf     :: Bsdf
   , _vpoint    :: Point
   , _vnormal   :: Normal
   , _vwi       :: Vector
   , _vwo       :: Vector
   }

type Path = [Vertex]

-- | generates the eye path
eyePath :: Scene -> Ray -> Sampled Path
eyePath s r = nextVertex s wi int 0 where
   wi = normalize $ (-(rayDir r))
   int = s `intersect` r

-- | generates the light path
lightPath :: Scene -> Sampled Path
lightPath s = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay s ul ulo uld
   let wo = normalize $ rayDir ray --  (sScale li (absDot nl wo / pdf))
   nextVertex s (-wo) (s `intersect` ray) 0
   
connect :: Path -> Path -> WeightedSpectrum
connect ep lp = (1, white)

evalPath :: [Vertex] -> Sampled WeightedSpectrum
evalPath vs = do
   return (1, fromRGB (1, 1, 1))

nextVertex
   :: Scene
   -> Vector
   -> Maybe Intersection
   -> Int -- ^ depth
   -> Sampled Path
-- nothing hit, terminate path
nextVertex _ _ Nothing _ = return []
nextVertex sc wi (Just int) depth = do
   ubc <- rnd -- bsdf component
   ubd <- rnd2D -- bsdf dir
   
   let (BsdfSample _ spdf bf wo) = sampleBsdf bsdf wi ubc ubd
   let int' = intersect sc $ Ray p wo epsilon infinity
   let wi' = -wo
   
   x <- rnd -- russian roulette
   let rest = if x > pcont
               then return []
               else nextVertex sc wi' int' (depth + 1)
   
   (liftM . (:)) (Vert bsdf p n wi wo) $! rest
   
   where
      pcont = if depth > 3 then 0.5 else 1
      dg = intGeometry int
      bsdf = intBsdf int
      n = normalize $ dgN dg
      p = dgP dg
      

tracePath :: Scene -> Ray -> Sampled Path
tracePath s r = evalInt s (-(rayDir r)) (s `intersect` r)

evalInt :: Scene -> Vector -> Maybe Intersection -> Sampled Path
evalInt _ _ Nothing = return []
evalInt s wo (Just int) = do
   bsdfDirU <- rnd2D
   bsdfCompU <- rnd
   let (BsdfSample smpType spdf f wi) = sampleBsdf bsdf wo bsdfCompU bsdfDirU
   (liftM . (:)) (Vert bsdf p n wi wo) $ tracePath s (Ray p wi epsilon infinity) where
      bsdf = intBsdf int
      dg = intGeometry int
      p = dgP dg
      n = dgN dg

directLight :: Scene -> Ray -> Spectrum
directLight s ray = V.foldl (+) black (V.map (`le` ray) (sceneLights s))
