
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
import Graphics.Bling.Spectrum

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
   maybe (return $! (0, black)) ls (s `intersect` r) where
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
            l = sampleOneLight s p n wo bsdf $ RLS uln uld ubc ubd

         -- trace rays for specular reflection and transmission
         re <- cont (d+1) md s bsdf wo $ mkBxdfType [Specular, Reflection]
         tr <- cont (d+1) md s bsdf wo $ mkBxdfType [Specular, Transmission]
             
         return $! (1, l + re + tr + intLe int wo)

cont :: Int -> Int -> Scene -> Bsdf -> Vector -> BxdfType -> Sampled s Spectrum
cont d md s bsdf wo t
   | d == md = return $! black
   | otherwise = do
      let
         ray = Ray p wi epsilon infinity
         (BsdfSample _ pdf f wi) = sampleAdjBsdf' t bsdf wo 0.5 (0.5, 0.5)
         p = bsdfShadingPoint bsdf
         n = bsdfShadingNormal bsdf

      if (pdf == 0)
         then return $! black
         else do
            l <- directLighting d md s ray
            return $! sScale (f * snd l) (wi `absDot` n / pdf)
