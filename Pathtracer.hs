module Pathtracer(pathTracer) where

import Control.Monad
import Maybe(isJust, fromJust)

import Bsdf
import Geometry
import Light
import Material
import Math
import Random

-- pathTracer :: Integrator
pathTracer :: (Intersectable i, Light l) => Ray -> i -> [l] -> Rand Spectrum
pathTracer r s lights = nextVertex 0 True r (intersect r s) white black where
   nextVertex :: Int -> Bool -> Ray -> Maybe Intersection -> Spectrum -> Spectrum -> Rand Spectrum
   nextVertex 10 _ _ _ _ l = return l -- hard bound
   nextVertex _ True ray Nothing throughput l = -- nothing hit, specular bounce
      return (add l $ sScale throughput $ directLight ray)
   
   nextVertex _ False _ Nothing _ l = -- nothing hit, non-specular bounce
      return l
   
   nextVertex depth specBounce (Ray _ rd _ _) (Just int@(Intersection _ pos _)) throughput l = do
      (BsdfSample bsdfType pdf _ wi) <- sampleBsdf bsdf wo
      lHere <- sampleOneLight s lights int wo bsdf
      
      outRay <- return (Ray pos wi epsilon infinity)
      throughput' <- return $ sScale throughput $ f wi
      l' <- return $ add l (sScale throughput lHere)
      -- (nextVertex (depth + 1) False outRay (intersect outRay s) throughput' l')
      return $! wo
      where
            wo = neg rd
            mat = defaultMaterial
            f = evalBsdf bsdf wo
            bsdf = materialBsdf mat int
         
   directLight :: Ray -> Spectrum
   directLight r = foldl add black (map (\l -> lightEmittance l r) lights)

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
