
module Graphics.Bling.Integrator.BidirPath (
   BidirPath, mkBidirPathIntegrator
   ) where



import Debug.Trace
import Data.BitSet
import Control.Monad (liftM)
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
   sampleCount1D _ = 0
   sampleCount2D _ = 0
   
   contrib (BDP) s addContrib' r = do
      ep <- eyePath s r
      lp <- lightPath s
      
   --   cws <- (mkContrib $ aws (contribS0 ep + contribS1 ep) (connect ep lp))
      
      contribS0 ep >>= addContrib
      contribS1 s ep >>= addContrib
      
      where
         addContrib = liftSampled . addContrib'
         
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
   , _valpha   :: Spectrum
   }

type Path = [Vertex]

--------------------------------------------------------------------------------
-- Path Generation
--------------------------------------------------------------------------------

-- | generates the eye path
eyePath :: Scene -> Ray -> Sampled m Path
eyePath s r = nextVertex s wi int white 0 where
   wi = normalize $ (-(rayDir r))
   int = s `intersect` r

-- | generates the light path
lightPath :: Scene -> Sampled m Path
lightPath s = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay s ul ulo uld
   let wo = - (normalize $ rayDir ray)
   nextVertex s wo (s `intersect` ray) (sScale li (absDot nl wo / pdf)) 0
   
nextVertex
   :: Scene
   -> Vector
   -> Maybe Intersection
   -> Spectrum -- ^ alpha
   -> Int -- ^ depth
   -> Sampled m Path
-- nothing hit, terminate path
nextVertex _ _ Nothing _ _ = return []
nextVertex sc wi (Just int) alpha depth = do
   ubc <- rnd -- bsdf component
   ubd <- rnd2D -- bsdf dir

   let (BsdfSample t spdf f wo) = sampleBsdf bsdf wi ubc ubd
   let int' = intersect sc $ Ray p wo epsilon infinity
   let wi' = -wo
   let l = intLe int wo
   let pathScale = sScale f $ absDot wo (bsdfShadingNormal bsdf) / spdf
   let alpha' = pathScale * alpha
   x <- rnd -- russian roulette
   let rest = if x > pcont
               then return []
               else nextVertex sc wi' int' alpha' (depth + 1)

   if isBlack f || spdf == 0
      then return []
      else (liftM . (:)) (Vert bsdf p n wi wo l t alpha) $! rest

   where
      pcont = if depth > 3 then 0.5 else 1
      dg = intGeometry int
      bsdf = intBsdf int

      n = normalize $ dgN dg
      p = dgP dg

--------------------------------------------------------------------------------
-- Path Evaluation
--------------------------------------------------------------------------------
   
-- | contribution for the eye subpath randomly intersecting a light
--   source, or for light sources visible through specular reflections
contribS0 :: Path -> Sampled s Contribution
contribS0 p = mkContrib (1, s0 p) False where
   s0 [] = black
   s0 ((Vert _ _ _ _ _ l t f) : vs)
      | Specular `member` t = f * l + s0 vs
      | otherwise = f * l

-- | one light subpaths
contribS1 :: Scene -> Path -> Sampled s Contribution
contribS1 scene path = s1 path >>= \x -> mkContrib (1, x) False where
   s1 [] = return black
   s1 ((Vert bsdf p n _ wo _ t f) : vs)
      | Specular `member` t = s1 vs
      | otherwise = do

         -- for light sampling
         lNumU <- rnd
         lDirU <- rnd2D
         lBsdfCompU <- rnd
         lBsdfDirU <- rnd2D
         let lHere = sampleOneLight scene p n wo bsdf $ RLS lNumU lDirU lBsdfCompU lBsdfDirU
         return $ lHere
   
-- | follow specular paths from the light and connect the to the eye
contribT1 :: Scene -> Path -> Spectrum -> Consumer m -> Sampled m ()
contribT1 _ [] _ _ = return ()
contribT1 sc ((Vert bsdf p n wi _ _ t f):vs) li tell
   | Specular `member` t = contribT1 sc vs (f * li) tell
   | otherwise = liftSampled $ tell (True, smp)
   where
      le = li * evalBsdf bsdf wi we
      smp = ImageSample px py (absDot n we / (cPdf * d2), le * csf)
      (CameraSampleResult csf pCam px py cPdf) = sampleCam (sceneCam sc) p
      dCam = pCam - p
      we = normalize $ dCam
      d2 = sqLen dCam -- ^ distance squared

connect :: Path -> Path -> WeightedSpectrum
connect ep lp = (1, black)
