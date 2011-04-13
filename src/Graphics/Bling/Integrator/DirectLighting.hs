
module Graphics.Bling.Integrator.DirectLighting (
   DirectLighting, mkDirectLightingIntegrator
   ) where

import qualified Text.PrettyPrint as PP

import Graphics.Bling.Integrator
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data DirectLighting = DirectLighting {
   _sampleAll :: Bool
   }

-- | creates an instance of @DirectLighting@
mkDirectLightingIntegrator :: Bool -> DirectLighting
mkDirectLightingIntegrator = DirectLighting

instance SurfaceIntegrator DirectLighting where
   pp dl = PP.text "Direct Lighting" 
   li (DirectLighting sa) = directLighting sa


directLighting :: Bool -> Scene -> Ray -> Rand WeightedSpectrum
directLighting sa s r@(Ray _ rd _ _) =
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
      