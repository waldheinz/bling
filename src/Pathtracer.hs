
module Pathtracer (pathTracer) where

import Data.BitSet
import Data.Array.Unboxed
import Debug.Trace

import Geometry
import Light
import Math
import Primitive
import Random
import Scene
import Spectrum
import Transport

pathTracer :: Integrator
pathTracer scene r = nextVertex scene 0 True r (primIntersect (scenePrim scene) r) white black

directLight :: Scene -> Ray -> Spectrum
directLight s ray = foldl (+) black (map (`lightEmittance` ray) (elems $ sceneLights s))

nextVertex :: Scene -> Int -> Bool -> Ray -> Maybe Intersection -> Spectrum -> Spectrum -> Rand WeightedSpectrum
-- nextVertex _ 10 _ _ _ _ l = return $! (1.0, seq l l) -- hard bound

nextVertex s _ True ray Nothing throughput l = -- nothing hit, specular bounce
   return $! (1.0, l + throughput * directLight s ray)
   
nextVertex _ _ False _ Nothing _ l = -- nothing hit, non-specular bounce
   return $! (1.0, seq l l)
   
nextVertex scene depth specBounce (Ray _ rd _ _) (Just int) throughput l 
   | isBlack throughput = return (1.0, l)
   | otherwise = do
      bsdfDirU <- rnd2D
      bsdfCompU <- rnd
      let (BsdfSample smpType pdf f wi) = sampleBsdf bsdf wo bsdfCompU bsdfDirU
      ulNum <- rnd
      lHere <- sampleOneLight scene p n wo bsdf ulNum
      let outRay = (Ray p wi epsilon infinity)
      let throughput' = throughput * sScale f (absDot wi n / pdf)
      let l' = l + (throughput * (lHere + intl))
      let spec' = Specular `member` smpType
      
      x <- rnd
      if x > pCont || (pdf == 0.0)
         then return $! (1.0, l')
         else nextVertex scene (depth + 1) spec' outRay (primIntersect (scenePrim scene) outRay) throughput' l'
         
      where
         dg = intGeometry int
         pCont = if depth <= 3 then 1 else min 0.5 (sY throughput)
         intl = if specBounce then intLe int wo else black
         wo = neg rd
         bsdf = intBsdf int
         n = dgN dg
         p = dgP dg
         
