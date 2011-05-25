
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
   contrib (BDP) s r = do
      e <- eyePath s r
      l <- lightPath s
      
      mkContrib $ aws (contribS0 e + contribS1 e) (connect e l)

aws :: Spectrum -> WeightedSpectrum -> WeightedSpectrum
aws s1 (w, s2) = (w, s1 + s2)

-- | a path vertex
data Vertex = Vert
   { _vbsdf    :: Bsdf
   , _vpoint   :: Point
   , _vnormal  :: Normal
   , _vwi      :: Vector
   , _vwo      :: Vector
   , _vle      :: Spectrum -- ^ light emitted here
   , _vtype    :: BxdfType
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


-- | contribution for when the eye subpath randomly intersects a light
--   source, or for light sources visible through specular reflections
contribS0 :: Path -> Spectrum
contribS0 [] = black
contribS0 ((Vert _ _ _ _ _ l t):vs) = go where
   go
      | Specular `member` t = l + contribS0 vs
      | otherwise = l
   
contribS1 :: Path -> Spectrum
contribS1 [] = black
contribS1 ((Vert _ _ _ _ _ _ _):vs) = black
   
contribT1 :: Path -> Sampled ()
contribT1 _  = return ()
--contribT1
   
connect :: Path -> Path -> WeightedSpectrum
connect ep lp = (1, black)

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
   
   let (BsdfSample t spdf bf wo) = sampleBsdf bsdf wi ubc ubd
   let int' = intersect sc $ Ray p wo epsilon infinity
   let wi' = -wo
   let l = intLe int wo
   x <- rnd -- russian roulette
   let rest = if x > pcont
               then return []
               else nextVertex sc wi' int' (depth + 1)
   
   (liftM . (:)) (Vert bsdf p n wi wo l t) $! rest
   
   where
      pcont = if depth > 3 then 0.5 else 1
      dg = intGeometry int
      bsdf = intBsdf int
      
      n = normalize $ dgN dg
      p = dgP dg
      