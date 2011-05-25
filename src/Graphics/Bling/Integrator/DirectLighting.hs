
module Graphics.Bling.Integrator.DirectLighting (
   DirectLighting, mkDirectLightingIntegrator
   ) where

import qualified Text.PrettyPrint as PP

import Graphics.Bling.Integrator
import Graphics.Bling.Math
import Graphics.Bling.Primitive
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
   contrib (DirectLighting sa) s r = directLighting sa s r >>= mkContrib

directLighting :: Bool -> Scene -> Ray -> Sampled WeightedSpectrum
directLighting _ s r@(Ray _ rd _ _) =
   maybe (return (0, black)) ls (s `intersect` r) where
      ls int = do
         ul <- rnd
         l <- sampleOneLight s p n wo bsdf ul
         return (1, l + intLe int wo) where
            dg = intGeometry int
            bsdf = intBsdf int
            p = dgP dg
            n = dgN dg
            wo = -rd
      