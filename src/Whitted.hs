module Whitted (whitted) where

import Data.Maybe(isJust, fromJust)

import Math
import Primitive
import Random
import Scene
import Spectrum

whitted :: Integrator
whitted sc ray@(Ray _ rd _ _) 
   | isJust mint = evalInt $ fromJust mint
   | otherwise = return (1, black) -- $! direct ray
   where
      wo = neg rd
      mint = intersect (scenePrim sc) ray

--      direct :: Ray -> Spectrum
--      direct _ [] = black
--      direct (l:ls) = (lightEmittance l ray) `add` (direct ls)
         
      evalInt :: Intersection -> Rand (Flt, Spectrum)
      evalInt int@(Intersection _ (DifferentialGeometry p n) _ _) = do
         l <- return black -- sampleAllLights sc p n wo (intBsdf int)
         return $! (1, l + intLe int wo)
