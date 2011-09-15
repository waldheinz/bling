
module Graphics.Bling.Integrator.BidirPath (
   BidirPath, mkBidirPathIntegrator
   ) where

import Data.BitSet
import Control.Monad (liftM)
import Control.Monad.Primitive
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
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
      ep <- eyePath s r
      (lp, li) <- lightPath s
      
      cEye <- mkContrib $ aws (contribS0 ep + contribS1 ep) (connect ep lp)
      ct1 <- contribT1 s lp li
      
      return $ cEye ++ ct1

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
   , _vf       :: Spectrum -- ^ sampled transport from wo to wi
   }

type Path = [Vertex]

-- | generates the eye path
eyePath :: PrimMonad m =>  Scene -> Ray -> Sampled m Path
eyePath s r = nextVertex s wi int 0 where
   wi = normalize $ (-(rayDir r))
   int = s `intersect` r

-- | generates the light path together with the emitted spectrum
lightPath :: PrimMonad m => Scene -> Sampled m (Path, Spectrum)
lightPath s = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay s ul ulo uld
   let wo = - (normalize $ rayDir ray)
   path <- nextVertex s wo (s `intersect` ray) 0
   return (path, sScale li (absDot nl wo / pdf))
   
-- | contribution for when the eye subpath randomly intersects a light
--   source, or for light sources visible through specular reflections
contribS0 :: Path -> Spectrum
contribS0 [] = black
contribS0 ((Vert _ _ _ _ _ l t f):vs) = go where
   l' = f * l
   go
      | Specular `member` t = l' + contribS0 vs
      | otherwise = l'
   
contribS1 :: Path -> Spectrum
contribS1 _ = black
-- contribS1 ((Vert _ _ _ _ _ _ _ _):vs) = black

-- | follow specular paths from the light and connect the to the eye
contribT1 :: PrimMonad m => Scene -> Path -> Spectrum -> Sampled m Contribution
contribT1 _ [] _ = return []
contribT1 sc ((Vert bsdf p n wi _ _ t f):vs) li
   | Specular `member` t = contribT1 sc vs (f * li)
   | otherwise = return [smp] 
   where
      le = li * evalBsdf bsdf wi we
      smp = ImageSample px py (absDot n we / (cPdf * d2), le * csf)
      (CameraSample csf pCam px py cPdf) = sampleCam (sceneCam sc) p
      dCam = pCam - p
      we = normalize $ dCam
      d2 = sqLen dCam -- ^ distance squared

connect :: Path -> Path -> WeightedSpectrum
connect ep lp = (1, black)

nextVertex
   :: PrimMonad m
   => Scene
   -> Vector
   -> Maybe Intersection
   -> Int -- ^ depth
   -> Sampled m Path
-- nothing hit, terminate path
nextVertex _ _ Nothing _ = return []
nextVertex sc wi (Just int) depth = do
   ubc <- rnd -- bsdf component
   ubd <- rnd2D -- bsdf dir
   
   let (BsdfSample t spdf f wo) = sampleBsdf bsdf wi ubc ubd
   let int' = intersect sc $ Ray p wo epsilon infinity
   let wi' = -wo
   let l = intLe int wo
   x <- rnd -- russian roulette
   let rest = if x > pcont
               then return []
               else nextVertex sc wi' int' (depth + 1)
   
   (liftM . (:)) (Vert bsdf p n wi wo l t f) $! rest
   
   where
      pcont = if depth > 3 then 0.5 else 1
      dg = intGeometry int
      bsdf = intBsdf int
      
      n = normalize $ dgN dg
      p = dgP dg
      