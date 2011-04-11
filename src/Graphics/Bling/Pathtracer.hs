
module Graphics.Bling.Pathtracer (pathTracer) where

import Data.BitSet
import Data.Vector.Generic as V

import Graphics.Bling.Light
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Reflection
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

pathTracer :: Integrator
pathTracer scene r = nextVertex scene 0 True r (intersect (scenePrim scene) r) white black

directLight :: Scene -> Ray -> Spectrum
directLight s ray = V.foldl (+) black (V.map (`le` ray) (sceneLights s))

nextVertex :: Scene -> Int -> Bool -> Ray -> Maybe Intersection -> Spectrum -> Spectrum -> Rand WeightedSpectrum
-- nextVertex _ 1 _ _ _ _ l = return $! (1.0, seq l l) -- hard bound

nextVertex s _ True ray Nothing throughput l = -- nothing hit, specular bounce
   return $! (1.0, l + throughput * directLight s ray)
   
nextVertex _ _ False _ Nothing _ l = -- nothing hit, non-specular bounce
   return $! (1.0, seq l l)
   
nextVertex scene depth specBounce (Ray _ rd _ _) (Just int) throughput l 
   | isBlack throughput = return (1.0, l)
   | otherwise = do
      bsdfDirU <- rnd2D
      bsdfCompU <- rnd
      let (BsdfSample smpType spdf f wi) = sampleBsdf bsdf wo bsdfCompU bsdfDirU
      ulNum <- rnd
      lHere <- sampleOneLight scene p n wo bsdf ulNum
      let l' = l + (throughput * (lHere + intl))
      
      x <- rnd
      if x > pCont || (spdf == 0.0)
         then return $! (1.0, l')
         else let
                 t' = throughput * sScale f (absDot wi n / (spdf * pCont))
                 s' = Specular `member` smpType
                 ray' = (Ray p wi epsilon infinity)
                 int' = intersect (scenePrim scene) ray'
              in nextVertex scene (depth + 1) s' ray' int' t' l'
      
      where
         dg = intGeometry int
         pCont = if depth <= 3 then 1 else min 0.5 (sY throughput)
         intl = if specBounce then intLe int wo else black
         wo = -rd
         bsdf = intBsdf int
         n = dgN dg
         p = dgP dg
