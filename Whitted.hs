module Whitted (whitted) where

import Maybe(isJust, fromJust)

import Color
import Geometry
import Math
import Primitive
import Random
import Scene

whitted :: Scene -> Ray -> Rand Spectrum
whitted scene@(Scene sp _) ray@(Ray _ rd _ _) 
   | isJust mint = evalInt $ fromJust mint
   | otherwise = return black -- $! direct ray
   where
      wo = neg rd
      
      mint :: Maybe Intersection
      mint = primIntersect sp ray
         
--      direct :: Ray -> Spectrum
--      direct _ [] = black
--      direct (l:ls) = (lightEmittance l ray) `add` (direct ls)
      
      evalInt :: Intersection -> Rand Spectrum
      evalInt int@(Intersection _ (DifferentialGeometry p n) _) = do
         l <- sampleAllLights scene p n wo (intBsdf int)
         return $! l + intLe int wo
      