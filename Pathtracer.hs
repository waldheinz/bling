module Pathtracer where

import Geometry
import Light
import Material
import Math
import Random

-- pathTracer :: Integrator
pathTracer :: (Intersectable i, Light l) => Ray -> i -> [l] -> Rand Spectrum
pathTracer r shape lights = pathTracer' shape lights (intersect r shape) 0

pathTracer' :: (Intersectable i, Light a) => i -> [a] -> Maybe Intersection -> Int -> Rand Spectrum
pathTracer' _ _ Nothing _ = return black
pathTracer' shape lights (Just int) depth = do
   
   y <- sampleOneLight shape lights int
   recurse <- keepGoing pc
   nextY <- if (recurse) then pathReflect shape lights int (depth + 1) else return black
   return $! (add y (scalMul nextY (1 / pc)))
   where
      pc = pCont depth
      coords = coordinates int
      
pathReflect :: (Intersectable i, Light a) => i -> [a] -> Intersection -> Int -> Rand Spectrum
pathReflect shape lights (Intersection _ pos n) depth = do
   rayOut <- reflect n pos
   yIn <- pathTracer' shape lights (intersect rayOut shape) depth
   return yIn
   -- return (scalMul yIn (geomFac n ((\(_, d) -> neg d) rayOut))) TODO
   
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
