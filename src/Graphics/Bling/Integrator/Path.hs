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
   
   contrib (PathIntegrator md _) s addSample r = {-# SCC "contrib" #-} do
      li <- nextVertex s 0 True r (s `intersect` r) md
      c <- mkContrib (1, li) False
      liftSampled $ addSample c
      
nextVertex :: Scene -> Int -> Bool -> Ray -> Maybe Intersection -> Int -> Sampled m Spectrum

-- nothing hit, specular bounce
nextVertex s _ True ray Nothing _ = return $ (V.sum $ V.map (`le` ray) (sceneLights s))
   
-- nothing hit, non-specular bounce
nextVertex _ _ False _ Nothing _ = return black

nextVertex scene depth spec (Ray _ rd _ _) (Just int) md
   | depth == md = return black
   | otherwise = do

      -- for light sampling
      lNumU <- rnd' $ 1 + smp1doff depth
      lDirU <- rnd2D' $ 1 + smp2doff depth
      lBsdfCompU <- rnd' $ 2 + smp1doff depth
      lBsdfDirU <- rnd2D' $ 2 + smp2doff depth

      let lHere = intl + (sampleOneLight scene p n wo bsdf $ RLS lNumU lDirU lBsdfCompU lBsdfDirU)
   
      -- for bsdf sampling
      bsdfCompU <- rnd' $ 0 + smp1doff depth
      bsdfDirU <- rnd2D' $ 0 + smp2doff depth
      
      let (BsdfSample smpType spdf f wi) = sampleBsdf bsdf wo bsdfCompU bsdfDirU

      if spdf == 0 || isBlack f
         then return lHere
         else let
                  t = sScale f (absDot wi n / spdf)
                  pc = if depth <= 3 then 1 else min 1 (sY t) -- cont. probability
                  spec' = Specular `member` smpType
                  ray' = (Ray p wi epsilon infinity)
                  int' = scene `intersect` ray'
                  depth' = depth + 1
                  rest x = if x > pc
                            then return black
                            else nextVertex scene depth' spec' ray' int' md
                            
              in do
                  r <- rnd' (3 + smp1doff depth) >>= rest
                  return $ t * (lHere + sScale r (1 / pc))
      
      where
         dg = intGeometry int
         intl = if spec then intLe int wo else black
         wo = -rd
         bsdf = intBsdf int
         n = dgN dg
         p = dgP dg
