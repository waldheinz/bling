
module Graphics.Bling.Integrator.BidirPath (
   mkBidirPathIntegrator, mkNoDirectBidirIntegrator
   ) where

import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Control.Monad (liftM, forM, forM_, when)
import Control.Monad.ST

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene

-- | a path vertex
data Vertex = Vert
   { _vwi      :: {-# UNPACK #-} !Vector
   , _vwo      :: {-# UNPACK #-} !Vector
   , _vint     :: !Intersection
   , _vtype    :: {-# UNPACK #-} !BxdfType
   , _valpha   :: !Spectrum
   }

type Path = [Vertex]

smps2D :: Int
smps2D = 3

smps1D :: Int
smps1D = 4

-- | a bi-directional path integrator which skips the direct lighting
mkNoDirectBidirIntegrator :: Int -> Int -> SurfaceIntegrator
mkNoDirectBidirIntegrator md sd = SurfaceIntegrator li s1d s2d where
   s1d = smps1D * sd * 3 + 1
   s2d = smps2D * sd * 3 + 2
   li = contrib True md

mkBidirPathIntegrator :: Int -> Int -> SurfaceIntegrator
mkBidirPathIntegrator md sd = SurfaceIntegrator li s1d s2d where
   s1d = smps1D * sd * 3 + 1
   s2d = smps2D * sd * 3 + 2
   li = contrib False md

contrib :: Bool -> Int -> Scene -> Ray -> Sampled s Spectrum
contrib noDirect md scene r = {-# SCC "contrib" #-} do
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
               else sum $ map ((\v -> _valpha v * intLe (_vint v) (_vwo v)) . fst)
                        $ filter snd $ zip ep prevSpec
      
       ei = zip ep [0..]
       li = zip lp [0..]
       l = sum $ map (connect scene nspecBouces) $ pairs ei li
   
   if null ep || null lp
      then return $! ld + le
      else return $! ld + le + l
      
--------------------------------------------------------------------------------
-- Path Evaluation
--------------------------------------------------------------------------------

-- compute number of specular vertices for each path length
countSpec :: Path -> Path -> V.Vector Float
countSpec ep lp = runST $ do
   x <- MV.replicate (length ep + length lp + 2) 0
   forM_ [0..length ep - 1] $ \i ->
      forM_ [0..length lp - 1] $ \j ->
         when (_vtype (ep !! i) `bxdfIs` Specular || _vtype (lp !! j) `bxdfIs` Specular) $
            MV.read x (i+j+2) >>= \old -> MV.write x (i+j+2) (old + 1)
   V.freeze x
   
connect :: Scene -> V.Vector Float -> ((Vertex, Int),  (Vertex, Int)) -> Spectrum
connect scene nspec
   ((Vert _ wie inte te alphae, i),  -- eye vertex
    (Vert _ wil intl tl alphal, j))  -- camera vertex
       | te `bxdfIs` Specular = black
       | tl `bxdfIs` Specular = black
       | isBlack fe || isBlack fl = black
       | scene `intersects` r = black
       | otherwise = sScale (alphae * fe * alphal * fl) (g * pathWt)
       where
          pathWt = 1 / (fromIntegral (i + j + 2) - nspec V.! (i+j+2))
          g = 1 / sqLen (pl - pe)
          (w, wl) = normLen $ pl - pe
          nspece = fromIntegral $ bsdfSpecCompCount bsdfe
          fe = sScale (evalBsdf False bsdfe wie w) (1 + nspece)
          nspecl = fromIntegral $ bsdfSpecCompCount bsdfl
          fl = sScale (evalBsdf True bsdfl wil (-w)) (1 + nspecl)
          r = Ray pe w (intEpsilon inte) (wl - intEpsilon intl)
          bsdfe = intBsdf inte
          bsdfl = intBsdf intl
          pe = bsdfShadingPoint bsdfe
          pl = bsdfShadingPoint bsdfl
          
estimateDirect :: Scene -> Vertex -> Int -> Sampled s Spectrum
estimateDirect scene (Vert wi _ int _ alpha) depth = do
   lNumU <- rnd' $ 0 + 1 + smps1D * depth * 3
   lDirU <- rnd2D' $ 0 + 2 + smps2D * depth * 3
   lBsdfCompU <- rnd' $ 1 + 1 + smps1D * depth * 3
   lBsdfDirU <- rnd2D' $ 1 + 2 + smps2D * depth * 3
   let
      bsdf = intBsdf int
      p = bsdfShadingPoint bsdf
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
   wi = -(rayDir r)
   int = s `intersect` r

-- | generates the light path
lightPath :: Scene -> Int -> Float -> Rand2D -> Rand2D -> Sampled m Path
lightPath s md ul ulo uld =
   let
      (li, ray, nl, pdf) = sampleLightRay s ul ulo uld
      li' = sScale li (absDot nl wo / pdf)
      wo = -(rayDir ray)
   in nextVertex True s wo (s `intersect` ray) li' 0 md (\d -> 3 + 1 + smps1D * d * 3) (\d -> 3 + 2 + smps2D * d * 3)
   
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
      let vHere = Vert wi wo int t alpha
      let pathScale = f -- sScale f $ absDot wo (bsdfShadingNormal bsdf) / spdf
      let rrProb = 1 -- min 1 $ sY pathScale
      let alpha' = sScale (pathScale * alpha) (1 / rrProb)
     
      let rest = if rr > rrProb
                  then return [] -- terminate
                  else nextVertex adj sc wi' int' alpha' (depth + 1) md f1d f2d
   
      if isBlack f || spdf == 0
         then return [vHere]
         else (liftM . (:)) vHere $! rest
         
      where
         p = bsdfShadingPoint bsdf
         bsdf = intBsdf int
         
