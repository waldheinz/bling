
module Graphics.Bling.Integrator.DirectLighting (
   DirectLighting, mkDirectLightingIntegrator
   ) where

import qualified Text.PrettyPrint as PP

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene

data DirectLighting = DL { _maxDepth  :: {-# UNPACK #-} ! Int }

-- | creates an instance of @DirectLighting@
mkDirectLightingIntegrator
   :: Int -- ^ maximum depth
   -> DirectLighting
mkDirectLightingIntegrator = DL

instance Printable DirectLighting where
   prettyPrint _ = PP.text "Direct Lighting" 

instance SurfaceIntegrator DirectLighting where
   sampleCount1D (DL md) = 2 * md
   sampleCount2D (DL md) = 2 * md
   
   contrib (DL md) s addSample r = do
      c <- directLighting 0 md s r >>= \is -> mkContrib is False
      liftSampled $ addSample c

directLighting :: Int -> Int -> Scene -> Ray -> Sampled m WeightedSpectrum
directLighting d md s r@(Ray _ rd _ _) =
   maybe (return $! (WS 0 black)) ls (s `intersect` r) where
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
             
         return $! WS 1 $ l + re + tr + intLe int wo

cont :: Flt -> Int -> Int -> Scene -> Bsdf -> Vector -> BxdfType -> Sampled s Spectrum
cont e d md s bsdf wo t
   | d == md = return $! black
   | otherwise = do
      let
         (BsdfSample _ pdf f wi) = sampleAdjBsdf' t bsdf wo 0.5 (0.5, 0.5)
         p = bsdfShadingPoint bsdf
         n = bsdfShadingNormal bsdf
         ray = Ray p wi e infinity

      if (pdf == 0)
         then return $! black
         else do
            (WS _ l) <- directLighting d md s ray
            return $! sScale (f * l) (wi `absDot` n / pdf)
            
