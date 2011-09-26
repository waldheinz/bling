{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Bling.Integrator.Path (
   mkPathIntegrator, PathIntegrator
   ) where

import Data.BitSet
import qualified Data.Vector.Generic as V
import qualified Text.PrettyPrint as PP

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Light
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data PathIntegrator = PathIntegrator {-# UNPACK #-} !Int {-# UNPACK #-} !Int  

mkPathIntegrator
   :: Int -- ^ maximum depth
   -> Int -- ^ sample depth
   -> PathIntegrator
mkPathIntegrator m s = PathIntegrator m (min m s)

smps2D :: Int
smps2D = 3

smps1D :: Int
smps1D = 4

smp2doff :: Int -> Int
smp2doff d = smps2D * d

smp1doff :: Int -> Int
smp1doff d = smps1D * d

instance Printable PathIntegrator where
   prettyPrint (PathIntegrator md sd) =
      PP.text ("Path Integrator " ++ (show md) ++ " " ++ (show sd))

instance SurfaceIntegrator PathIntegrator where
   sampleCount1D (PathIntegrator _ sd) = smps1D * sd
   
   sampleCount2D (PathIntegrator _ sd) = smps2D * sd
   
   contrib (PathIntegrator md _) s addSample r = {-# SCC "pathContrib" #-} do
      li <- nextVertex s 0 True r (s `intersect` r) white md
      c <- mkContrib (1, li) False
      liftSampled $ addSample c
      
nextVertex :: Scene -> Int -> Bool -> Ray -> Maybe Intersection -> Spectrum -> Int -> Sampled m Spectrum

-- nothing hit, specular bounce
nextVertex s _ True ray Nothing t _  =
   {-# SCC "nextVertex.termSpec" #-} return $ t * (V.sum $ V.map (`le` ray) (sceneLights s))
   
-- nothing hit, non-specular bounce
nextVertex _ _ False _ Nothing _ _ = return black

nextVertex scene depth spec (Ray _ rd _ _) (Just int) t md
   | isBlack t || depth == md = return black
   | otherwise = do
      -- for bsdf sampling
      bsdfCompU <- rnd' $ 0 + smp1doff depth
      bsdfDirU <- rnd2D' $ 0 + smp2doff depth

      -- for light sampling
      lNumU <- rnd' $ 1 + smp1doff depth
      lDirU <- rnd2D' $ 1 + smp2doff depth
      lBsdfCompU <- rnd' $ 2 + smp1doff depth
      lBsdfDirU <- rnd2D' $ 2 + smp2doff depth
      
      let (BsdfSample smpType spdf f wi) = sampleBsdf bsdf wo bsdfCompU bsdfDirU
      let lHere = sampleOneLight scene p n wo bsdf $ RLS lNumU lDirU lBsdfCompU lBsdfDirU
      let l = t * (lHere + intl)
      
      rnd' (3 + smp1doff depth) >>= \x -> if x > pc || (spdf == 0) || isBlack f
         then return black
         else let
                 t' = t * sScale f (absDot wi n / (spdf * pc))
                 spec' = Specular `member` smpType
                 ray' = (Ray p wi epsilon infinity)
                 int' = {-# SCC "nextVertex.intersect" #-} intersect (scenePrim scene) ray'
                 depth' = depth + 1
                 rest = nextVertex scene depth' spec' ray' int' t' md
              in do
                 r <- rest
                 return $ r + l
      
      where
         dg = intGeometry int
         pc = if depth <= 3 then 1 else min 1 (sY t) -- cont. probability
         intl = if spec then intLe int wo else black
         wo = -rd
         bsdf = intBsdf int
         n = dgN dg
         p = dgP dg
