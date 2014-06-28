
module Graphics.Bling.Integrator.DirectLighting (
   mkDirectLightingIntegrator
   ) where

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene

-- | creates an instance of @DirectLighting@
mkDirectLightingIntegrator
   :: Int -- ^ maximum depth
   -> SurfaceIntegrator
mkDirectLightingIntegrator md = SurfaceIntegrator li s1d s2d where
   s1d = 2 * md
   s2d = 2 * md
   
   li s r = directLighting 0 md s r
   
directLighting :: Int -> Int -> Scene -> Ray -> Sampled m Spectrum
directLighting d md s r@(Ray _ rd _ _) =
   maybe (return $! black) ls (s `scIntersect` r) where
      ls int = do
         uln <- rnd' $ 0 + (2 * d)
         uld <- rnd2D' $ 0 + (2 * d)
         ubc <- rnd' $ 1 + (2 * d)
         ubd <- rnd2D' $ 1 + (2 * d)
         let
            bsdf = intBsdf int
            p = bsdfShadingPoint bsdf
            n = bsdfShadingNormal bsdf
            wo = -rd
            e = intEpsilon int
            l = sampleOneLight s p e n wo bsdf $ RLS uln uld ubc ubd

         -- trace rays for specular reflection and transmission
         re <- cont e (d+1) md s bsdf wo $ mkBxdfType [Specular, Reflection]
         tr <- cont e (d+1) md s bsdf wo $ mkBxdfType [Specular, Transmission]
             
         return $! l + re + tr + intLe int wo

cont :: Float -> Int -> Int -> Scene -> Bsdf -> Vector -> BxdfType -> Sampled s Spectrum
cont e d md s bsdf wo t
   | d == md = return $! black
   | otherwise = do
      let
         (BsdfSample _ pdf f wi) = sampleBsdf' t bsdf wo 0.5 (0.5, 0.5)
         p = bsdfShadingPoint bsdf
         ray = Ray p wi e infinity

      if (pdf == 0)
         then return $! black
         else do
            l <- directLighting d md s ray
            return $! f * l
            
