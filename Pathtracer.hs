module Pathtracer(pathTracer) where

import Control.Monad

import Color
import Geometry
import Light
import Math
import Primitive
import Random
import Scene
import Transport

pathTracer :: Integrator
pathTracer s r = nextVertex 0 True r (primIntersect s r) white black where
   nextVertex :: Int -> Bool -> Ray -> Maybe Intersection -> Spectrum -> Spectrum -> Rand WeightedSpectrum
--   nextVertex _ _ (Ray ro rd _ _ ) _ _ _ = return rd
   nextVertex 10 _ _ _ _ l = return $! (1.0, seq l l) -- hard bound
   
   nextVertex _ True ray Nothing throughput l = -- nothing hit, specular bounce
      return $! (1.0, (l + throughput * directLight ray))
   
   nextVertex _ False _ Nothing _ l = -- nothing hit, non-specular bounce
      return $! (1.0, seq l l)
   
   nextVertex depth specBounce (Ray _ rd _ _) (Just int@(Intersection _ dg _)) throughput l 
      | isBlack throughput = return (1.0, l)
      | otherwise = do
         (BsdfSample _ pdf _ wi) <- sampleBsdf bsdf wo
         
         lHere <- sampleOneLight s p n wo bsdf
         
         outRay <- return (Ray p wi epsilon infinity)
         throughput' <- return $! throughput * sScale (f wi) ((absDot wi n) / pdf)
            
         l' <- return $! l + (throughput * (lHere + intl))
         
         x <- rnd
         if (x > pCont)
            then return $! (1.0, l)
            else nextVertex (depth + 1) False outRay (primIntersect s outRay) throughput' l'
            
         where
               pCont = if depth <= 3 then 1 else 0.5
               intl = if specBounce then intLe int wo else black
               f = evalBsdf bsdf wo
               wo = normalize $ neg rd
               bsdf = intBsdf int
               n = dgN dg
               p = dgP dg
         
   directLight :: Ray -> Spectrum
   directLight ray = foldl (+) black (map (\l -> lightEmittance l ray) (sceneLights s))

