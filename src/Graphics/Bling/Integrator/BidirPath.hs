
module Graphics.Bling.Integrator.BidirPath (
   BidirPath, mkBidirPathIntegrator
   ) where

import Data.BitSet
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Control.Monad (liftM, forM, forM_)
import Control.Monad.ST
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

mkBidirPathIntegrator :: BidirPath
mkBidirPathIntegrator = BDP

instance Printable BidirPath where
   prettyPrint (BDP) = PP.text "Bi-Dir Path"

instance SurfaceIntegrator BidirPath where
   sampleCount1D _ = 0
   sampleCount2D _ = 0
   
   contrib (BDP) scene addContrib' r = do
      ep <- eyePath scene r
      lp <- lightPath scene

      -- precompute sum of specular bounces in eye or light path
      let nspecBouces = countSpec ep lp
      
      -- direct illumination, aka "one light" or S1 subpaths
      ld <- liftM sum $ forM (zip ep [0..]) $ \(v, i) -> do
         d <- estimateDirect scene v
         return $ sScale d $ 1 / (fromIntegral i + 1 - (nspecBouces V.! (i+1)))

      let prevSpec = True : map (\v -> Specular `member` _vtype v) ep

      -- light sources directly visible, or by specular reflection
      let le = sum $ map (\v -> _vle v * _valpha v) $ map fst $ filter snd $ zip ep prevSpec
      
      let ei = zip ep [0..]
      let li = zip lp [0..]

      let l = sum $ map (connect scene nspecBouces) $ pairs ei li
      mkContrib (1, l + ld + le) False >>= addContrib
      where
         addContrib = liftSampled . addContrib'

--------------------------------------------------------------------------------
-- Path Evaluation
--------------------------------------------------------------------------------

-- compute number of specular vertices for each path length
countSpec :: Path -> Path -> V.Vector Flt
countSpec ep lp = runST $ do
   x <- MV.replicate (length ep + length lp + 2) 0
   forM_ [0..length ep - 1] $ \i -> do
      forM_ [0..length lp - 1] $ \j -> do
         if Specular `member` (_vtype $ ep !! i) || Specular `member` (_vtype $ lp !! j)
            then do
               old <- MV.read x (i+j+2)
               MV.write x (i+j+2) (old + 1)
            else return ()
   V.freeze x

connect :: Scene -> V.Vector Flt -> ((Vertex, Int),  (Vertex, Int)) -> Spectrum
connect scene nspec
   ((Vert bsdfe pe ne wie woe le te alphae, i),  -- eye vertex
    (Vert bsdfl pl nl wil wol ll tl alphal, j))   -- camera vertex
       | Specular `member` te = black
       | Specular `member` tl = black
       | isBlack fe || isBlack fl = black
       | scene `intersects` r = black
       | otherwise = sScale (alphae * fe * alphal * fl) (g * pathWt)
       where
          pathWt = 1 / (fromIntegral (i + j + 2) - nspec V.! (i+j+2))
          g = absDot ne w * absDot nl w / sqLen (pl - pe)
          w = normalize $ pl - pe
          nspece = fromIntegral $ bsdfSpecCompCount bsdfe
          fe = sScale (evalBsdf bsdfe woe  w) (1 + nspece)
          nspecl = fromIntegral $ bsdfSpecCompCount bsdfl
          fl = sScale (evalBsdf bsdfl (-w) wol) (1 + nspecl)
          r = segmentRay pl pe
          
estimateDirect :: Scene -> Vertex -> Sampled s Spectrum
estimateDirect scene (Vert bsdf p n wi wo l t alpha) = do
   lNumU <- rnd
   lDirU <- rnd2D
   lBsdfCompU <- rnd
   lBsdfDirU <- rnd2D
   let lHere = sampleOneLight scene p n wo bsdf $ RLS lNumU lDirU lBsdfCompU lBsdfDirU
   return $ lHere * alpha

pairs :: [a] -> [a] -> [(a, a)]
pairs [] _ = []
pairs _ [] = []
pairs (x:xs) ys = zip (repeat x) ys ++ pairs xs ys

-- | follow specular paths from the light and connect the to the eye
contribT1 :: Scene -> Path -> Consumer m -> Sampled m ()
contribT1 _ [] _ = return ()
contribT1 sc ((Vert bsdf p n _ wi _ t le):vs) tell
   | Specular `member` t = contribT1 sc vs tell
   | intersects sc ray = contribT1 sc vs tell
   | otherwise = liftSampled $ tell (True, smp)
   where
      f = csf * le * evalBsdf bsdf wi we
      g = absDot n wi * absDot n we / (cPdf * d2)
      smp = ImageSample px py (1 /g, f)
      ray = segmentRay p pCam
--      let smpHere = ImageSample px py (absDot n we / (cPdf * dCam2), f * li * csf)
      (CameraSampleResult csf pCam px py cPdf) = sampleCam (sceneCam sc) p
      dCam = pCam - p
      we = normalize $ dCam
      d2 = sqLen dCam -- ^ distance squared

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
   let wo = normalize $ rayDir ray
   nextVertex s (-wo) (s `intersect` ray) (sScale li (absDot nl wo / pdf)) 0
   
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
   let pathScale = sScale f $ absDot wo (bsdfShadingNormal bsdf) / (spdf * pcont)
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
