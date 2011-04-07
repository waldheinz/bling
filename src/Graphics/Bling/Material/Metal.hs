
module Graphics.Bling.Material.Metal (
   -- * Creating Metal Material
   mkMetal

   ) where


import Graphics.Bling.Texture
import Graphics.Bling.Reflection
import Graphics.Bling.Spectrum

-- | Creates a Metal @Material@
mkMetal
   :: SpectrumTexture -- ^ eta
   -> SpectrumTexture -- ^ k
   -> ScalarTexture -- ^ roughness
   -> Material

mkMetal eta k rough dg = mkBsdf [spec] sc where
   fr = frConductor (eta dg) (k dg)
   spec = MkAnyBxdf $ Microfacet (Blinn (1 / rough dg)) fr white
   sc = shadingCs dg
