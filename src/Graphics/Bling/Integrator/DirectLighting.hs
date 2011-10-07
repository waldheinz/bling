
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

data DirectLighting = DirectLighting {
   _sampleAll :: Bool
   }

-- | creates an instance of @DirectLighting@
mkDirectLightingIntegrator :: Bool -> DirectLighting
mkDirectLightingIntegrator = DirectLighting

instance Printable DirectLighting where
   prettyPrint _ = PP.text "Direct Lighting" 

instance SurfaceIntegrator DirectLighting where
   sampleCount1D _ = 2
   sampleCount2D _ = 2
   
   contrib (DirectLighting sa) s addSample r = do
      c <- directLighting sa s r >>= \is -> mkContrib is False
      liftSampled $ addSample c

directLighting :: Bool -> Scene -> Ray -> Sampled m WeightedSpectrum
directLighting _ s r@(Ray _ rd _ _) =
   maybe (return (0, black)) ls (s `intersect` r) where
      ls int = do
         uln <- rnd' 0
         uld <- rnd2D' 0
         ubc <- rnd' 1
         ubd <- rnd2D' 1
         let l = sampleOneLight s p n wo bsdf $ RLS uln uld ubc ubd 
         return (1, l + intLe int wo) where
            bsdf = intBsdf int
            p = bsdfShadingPoint bsdf
            n = bsdfShadingNormal bsdf
            wo = -rd
      