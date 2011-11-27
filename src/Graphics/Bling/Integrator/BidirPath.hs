
module Graphics.Bling.Integrator.BidirPath (
   BidirPath, mkBidirPathIntegrator, mkNoDirectBidirIntegrator
   ) where

import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad (liftM, forM, forM_)
import Control.Monad.ST
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Text.PrettyPrint as PP

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene

data BidirPath = BDP
   { _maxDepth    :: {-# UNPACK #-} ! Int
   , _sampleDepth :: {-# UNPACK #-} ! Int
   , _noDirect    :: ! Bool -- ^ should we skip direct illumination?
   }
   
-- | a path vertex
data Vertex = Vert
   { _vbsdf    :: Bsdf
   , _vpoint   :: Point
   , _vwi      :: Vector
   , _vwo      :: Vector
   , _vint     :: Intersection
   , _vtype    :: BxdfType
   , _valpha   :: Spectrum
   }

type Path = [Vertex]

mkBidirPathIntegrator :: Int -> Int -> BidirPath
mkBidirPathIntegrator md sd = BDP md sd False

-- | a bi-directional path integrator which skips the direct lighting
mkNoDirectBidirIntegrator :: Int -> Int -> BidirPath
mkNoDirectBidirIntegrator md sd = BDP md sd True

instance Printable BidirPath where
   prettyPrint (BDP _ _ _) = PP.text "Bi-Dir Path"

smps2D :: Int
smps2D = 3

smps1D :: Int
smps1D = 4

instance SurfaceIntegrator BidirPath where
   sampleCount1D (BDP _ sd _) = smps1D * sd * 3 + 1
   
   sampleCount2D (BDP _ sd _) = smps2D * sd * 3 + 2
   
   contrib (BDP md _ noDirect) scene addContrib' r = {-# SCC "contrib" #-} do
      ul <- rnd' 0
      ulo <- rnd2D' 0
      uld <- rnd2D' 1
      
      lp <- lightPath scene md ul ulo uld
      ep <- eyePath scene r md
      
      let
         -- precompute sum of specular bounces in eye or light path
         nspecBouces = countSpec ep lp
         
         -- if we do separate DL, we must drop the specular bounces at the
         -- beginning of the eye path to avoid double-counting
         ep' = if noDirect
                  then dropWhile (\v -> _vtype v `bxdfIs` Specular) $ tail ep
                  else ep
      
      -- direct illumination, aka "one light" or S1 subpaths
      ld <- liftM sum $ forM (zip ep' [0..]) $ \(v, i) -> do
         d <- estimateDirect scene v i
         return $ sScale d (1 / (1 + fromIntegral i - nspecBouces V.! i))
      
      let
          prevSpec = True : map (\v -> _vtype v `bxdfIs` Specular) ep

          -- light sources directly visible, or via specular reflection
          le = if noDirect
                  then black
                  else sum $ map (\v -> _valpha v * (intLe (_vint v) (_vwi v))) $
                             map fst $ filter snd $ zip ep prevSpec
      
          ei = zip ep [0..]
          li = zip lp [0..]
          l = sum $ parMap rdeepseq (connect scene nspecBouces) $ pairs ei li
          
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
         if (_vtype $ ep !! i) `bxdfIs` Specular || (_vtype $ lp !! j) `bxdfIs` Specular
            then do
               old <- MV.read x (i+j+2)
               MV.write x (i+j+2) (old + 1)
            else return ()
   V.freeze x

connect :: Scene -> V.Vector Flt -> ((Vertex, Int),  (Vertex, Int)) -> Spectrum
connect scene nspec
   ((Vert bsdfe pe _ wie inte te alphae, i),  -- eye vertex
    (Vert bsdfl pl _ wil intl tl alphal, j))  -- camera vertex
       | te `bxdfIs` Specular = black
       | tl `bxdfIs` Specular = black
       | isBlack fe || isBlack fl = black
       | scene `intersects` r = black
       | otherwise = sScale (alphae * fe * alphal * fl) (g * pathWt)
       where
          pathWt = 1 / (fromIntegral (i + j + 2) - nspec V.! (i+j+2))
          g = ((absDot ne w) * (absDot nl w)) / sqLen (pl - pe)
          w = normalize $ pl - pe
          nspece = fromIntegral $ bsdfSpecCompCount bsdfe
          fe = sScale (evalBsdf True bsdfe wie w) (1 + nspece)
          nspecl = fromIntegral $ bsdfSpecCompCount bsdfl
          fl = sScale (evalBsdf False bsdfl (-w) wil) (1 + nspecl)
          r = Ray pl rd (intEpsilon intl) (len rd - intEpsilon inte)
          rd = pe - pl
          ne = bsdfShadingNormal bsdfe
          nl = bsdfShadingNormal bsdfl
          
estimateDirect :: Scene -> Vertex -> Int -> Sampled s Spectrum
estimateDirect scene (Vert bsdf p wi _ int _ alpha) depth = do
   lNumU <- rnd' $ 0 + 1 + smps1D * depth * 3
   lDirU <- rnd2D' $ 0 + 2 + smps2D * depth * 3
   lBsdfCompU <- rnd' $ 1 + 1 + smps1D * depth * 3
   lBsdfDirU <- rnd2D' $ 1 + 2 + smps2D * depth * 3
   let
      n = bsdfShadingNormal bsdf
      lHere = sampleOneLight scene p (intEpsilon int) n wi bsdf $
         RLS lNumU lDirU lBsdfCompU lBsdfDirU
   return $! lHere * alpha
   
pairs :: [a] -> [a] -> [(a, a)]
pairs [] _ = []
pairs _ [] = []
pairs (x:xs) ys = zip (repeat x) ys ++ pairs xs ys

--------------------------------------------------------------------------------
-- Path Generation
--------------------------------------------------------------------------------

-- | generates the eye path
eyePath :: Scene -> Ray -> Int -> Sampled m Path
eyePath s r md = nextVertex False s wi int white 0 md (\d -> 2 + 1 + smps1D * d * 3) (\d -> 2 + 2 + smps2D * d * 3) where
   wi = normalize $ (-(rayDir r))
   int = s `intersect` r

-- | generates the light path
lightPath :: Scene -> Int -> Flt -> Rand2D -> Rand2D -> Sampled m Path
lightPath s md ul ulo uld =
   let
      (li, ray, nl, pdf) = sampleLightRay s ul ulo uld
      wo = normalize $ rayDir ray
      nl' = normalize nl
   in do
      nextVertex True s (-wo) (s `intersect` ray) (sScale li (absDot nl' wo / pdf)) 0 md (\d -> 3 + 1 + smps1D * d * 3) (\d -> 3 + 2 + smps2D * d * 3)
   
nextVertex
   :: Bool -- ^ adjoint ?
   -> Scene
   -> Vector
   -> Maybe Intersection
   -> Spectrum -- ^ alpha
   -> Int -- ^ depth
   -> Int -- ^ maximum depth
   -> (Int -> Int) -- ^ 1d offsets
   -> (Int -> Int) -- ^ 2d offsets
   -> Sampled m Path
-- nothing hit, terminate path
nextVertex _ _ _ Nothing _ _ _ _ _ = return []
nextVertex adj sc wi (Just int) alpha depth md f1d f2d
   | depth == md = return []
   | otherwise = do
      ubc <- rnd' $ f1d depth -- bsdf component
      ubd <- rnd2D' $ f2d depth -- bsdf dir
      rr <- rnd' $ 1 + f1d depth -- russian roulette
      let fun = if adj then sampleAdjBsdf else sampleBsdf
      let (BsdfSample t spdf f wo) = fun bsdf wi ubc ubd
      let int' = intersect sc $ Ray p wo (intEpsilon int) infinity
      let wi' = -wo
      let vHere = Vert bsdf p wi wo int t alpha
      let pathScale = sScale f $ absDot wo (bsdfShadingNormal bsdf) / spdf
      let rrProb = min 1 $ sY pathScale
      let alpha' = sScale (pathScale * alpha) (1 / rrProb)
      let rest = if rr > rrProb
                  then return [] -- terminate
                  else nextVertex adj sc wi' int' alpha' (depth + 1) md f1d f2d
   
      if isBlack f || spdf == 0
         then return [vHere]
         else (liftM . (:)) vHere $! rest
   
      where
         bsdf = intBsdf int
         p = bsdfShadingPoint bsdf
