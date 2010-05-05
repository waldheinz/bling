
module Pathtracer(pathTracer) where

import Control.Monad
import Data.BitSet

import Color
import Geometry
import Light
import Math
import Primitive
import Random
import Scene
import Transport

pathTracer :: Integrator
pathTracer s@(Scene p _) r = nextVertex s 0 True r (primIntersect p r) white black

directLight :: Scene -> Ray -> Spectrum
directLight s ray = foldl (+) black (map (\l -> lightEmittance l ray) (sceneLights s))

nextVertex :: Scene -> Int -> Bool -> Ray -> Maybe Intersection -> Spectrum -> Spectrum -> Rand WeightedSpectrum
nextVertex _ 10 _ _ _ _ l = return $! (1.0, seq l l) -- hard bound
   
nextVertex s _ True ray Nothing throughput l = -- nothing hit, specular bounce
   return $! (1.0, (l + throughput * directLight s ray))
   
nextVertex _ _ False _ Nothing _ l = -- nothing hit, non-specular bounce
   return $! (1.0, seq l l)
   
nextVertex s@(Scene sp _) depth specBounce (Ray _ rd _ _) (Just int@(Intersection _ dg _)) throughput l 
   | isBlack throughput = return (1.0, l)
   | otherwise = do
      bsdfDirU <- rnd2D
      bsdfCompU <- rnd
      (BsdfSample smpType pdf f wi) <- return $ sampleBsdf bsdf wo bsdfCompU bsdfDirU
      lHere <- sampleOneLight s p n wo bsdf
      outRay <- return (Ray p wi epsilon infinity)
      throughput' <- return $! throughput * sScale f ((absDot wi n) / pdf)
      
      l' <- return $! l + (throughput * (lHere + intl))
      spec' <- return $! (Specular `member` smpType)
      
      x <- rnd
      if (x > pCont)
         then return $! (1.0, l)
         else nextVertex s (depth + 1) spec' outRay (primIntersect sp outRay) throughput' l'
         
      where
            pCont = if depth <= 3 then 1 else 0.5
            intl = if specBounce then intLe int wo else black
            wo = normalize $ neg rd
            bsdf = intBsdf int
            n = dgN dg
            p = dgP dg
         
