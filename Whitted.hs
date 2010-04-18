module Whitted (whitted) where

import Maybe(isJust, fromJust)

import Geometry
import Light
import Material
import Math
import Random

whitted :: (Intersectable i, Light l) => Ray -> i -> [l] -> Rand Spectrum
whitted ray@(Ray _ rd _ _) scene lights
   | isJust mint = evalInt $ fromJust mint
   | otherwise = return $! (direct lights)
   where
      wo = neg rd
      
      mint :: Maybe Intersection
      mint = intersect ray scene
         
      direct :: (Light l) => [l] -> Spectrum
      direct [] = black
      direct (l:ls) = (lightEmittance l ray) `add` (direct ls)
      
      evalInt :: Intersection -> Rand Spectrum
      evalInt int = do
         l <- sampleAllLights scene lights int wo (materialBsdf defaultMaterial int)
         return l
      