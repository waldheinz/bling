module Pathtracer(pathTracer) where

import Maybe(isJust, fromJust)

import Geometry
import Light
import Material
import Math
import Random

-- pathTracer :: Integrator
pathTracer :: (Intersectable i, Light l) => Ray -> i -> [l] -> Rand Spectrum
pathTracer r s lights = nextVertex 0 True r white black where
   nextVertex :: Int -> Bool -> Ray -> Spectrum -> Spectrum -> Rand Spectrum
   nextVertex depth spec ray throughput l
      | (isJust mint) = evalInt ray (fromJust mint)
      | otherwise = return l'
         where
               mint = intersect ray s
               l' = if (spec || depth == 0)
                       then add l (directLight ray)
                       else l
               
   evalInt :: Ray -> Intersection -> Rand Spectrum
   evalInt (Ray _ rd _ _) int = do
      (BsdfSample wi pdf) <- materialSample mat int
      ls <- sampleOneLight s lights int
      return (sScale (f wi) ls)
      where
         mat = intMaterial int
         f = materialEval mat (neg rd)
         
         
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
