module Pathtracer(pathTracer) where

import Control.Monad
import Debug.Trace

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
      return $! (1.0, (add l $ sScale throughput $ directLight ray))
   
   nextVertex _ False _ Nothing _ l = -- nothing hit, non-specular bounce
      return $! (1.0, seq l l)
   
   nextVertex depth specBounce (Ray _ rd _ _) (Just int@(Intersection _ dg _)) throughput l 
      | isBlack throughput = return (1.0, l)
      | otherwise = do
         (BsdfSample _ pdf _ wi) <- sampleBsdf bsdf wo
         
         lHere <- sampleOneLight s p n wo bsdf
         
         outRay <- return (Ray p wi epsilon infinity)
         throughput' <- return $ sScale throughput $ scalMul (f wi) ((absDot wi n) / pdf)
            
         l' <- return $ add l (sScale throughput (add lHere intl))
            
         nextVertex (depth + 1) False outRay (primIntersect s outRay) throughput' l'
         where
               intl = if specBounce then intLe int wo else black
               f = evalBsdf bsdf wo
               wo = normalize $ neg rd
               bsdf = intBsdf int
               n = dgN dg
               p = dgP dg
         
   directLight :: Ray -> Spectrum
   directLight ray = foldl add black (map (\l -> lightEmittance l ray) (sceneLights s))

-- rolls a dice to decide if we should continue this path,
-- returning true with the specified probability
keepGoing :: Float -> Rand Bool
keepGoing 1 = return True
keepGoing pAbort = do
   x <- rnd
   return $! (x < pAbort)

-- probability for aborting at the given recursion depth
pCont :: Int -> Float
pCont d
   | d <= 3 = 1
   | otherwise = 0.5
