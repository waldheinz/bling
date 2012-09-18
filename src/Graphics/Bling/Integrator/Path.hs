{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Bling.Integrator.Path (
   mkPathIntegrator
   ) where

import qualified Data.Vector.Generic as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Light
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene

-- data PathIntegrator = PathIntegrator {-# UNPACK #-} !Int {-# UNPACK #-} !Int  


smps2D :: Int
smps2D = 3

smps1D :: Int
smps1D = 4

smp2doff :: Int -> Int
smp2doff d = smps2D * d

smp1doff :: Int -> Int
smp1doff d = smps1D * d

mkPathIntegrator
   :: Int -- ^ maximum depth
   -> Int -- ^ sample depth
   -> SurfaceIntegrator
mkPathIntegrator md sd = SurfaceIntegrator li s1d s2d where
   s1d = smps1D * sd
   s2d = smps2D * sd
   
   li s r = {-# SCC "contrib" #-}
      nextVertex s 0 True r (s `intersect` r) md white black
      
nextVertex :: Scene -> Int -> Bool -> Ray -> Maybe Intersection -> Int -> Spectrum -> Spectrum -> Sampled m Spectrum

-- nothing hit, specular bounce
nextVertex s _ True ray Nothing _ t l = {-# SCC "nothingSpec" #-} return $! l + t * (V.sum $ V.map (`le` ray) (sceneLights s))
   
-- nothing hit, non-specular bounce
nextVertex _ _ False _ Nothing _ _ l = return $! l

nextVertex scene depth spec (Ray _ rd _ _) (Just int) md t l
   | depth == md = return $! l
   | otherwise = do
      -- for light sampling
      lNumU <- rnd' $ 1 + smp1doff depth
      lDirU <- rnd2D' $ 1 + smp2doff depth
      lBsdfCompU <- rnd' $ 2 + smp1doff depth
      lBsdfDirU <- rnd2D' $ 2 + smp2doff depth
      
      let
         intl = if spec then intLe int wo else black 
         wo = -rd
         bsdf = intBsdf int
         n = bsdfShadingNormal bsdf
         p = bsdfShadingPoint bsdf
         eps = intEpsilon int
         lHere = intl + (sampleOneLight scene p eps n wo bsdf $ RLS lNumU lDirU lBsdfCompU lBsdfDirU)
         l' = l + t * lHere
         pc = if depth <= 7 then 1 else min 0.75 (sY t) -- cont. probability
        
      rnd' (3 + smp1doff depth) >>= \x -> do
         if x > pc
            then return $! l'
            else do
               uc <- rnd'   $ 0 + smp1doff depth
               ud <- rnd2D' $ 0 + smp2doff depth
               
               let
                  (BsdfSample smpType spdf f wi) = sampleBsdf bsdf wo uc ud
                  ray' = Ray p wi eps infinity
                  spec' = smpType `bxdfIs` Specular
                  t' = sScale (f * t) (1 / pc)
                  depth' = depth + 1
                  
               if spdf == 0 || isBlack f
                  then return $! l'
                  else nextVertex scene depth' spec' ray' (scene `intersect` ray') md t' l'
                  
