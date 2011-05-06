
module Graphics.Bling.Material.Metal (

   -- * Creating Metal Materials
   
   mkMetal, mkShinyMetal

   ) where


import Graphics.Bling.Texture
import Graphics.Bling.Reflection
import Graphics.Bling.Spectrum
import Graphics.Bling.Material.Specular

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

mkShinyMetal
   :: SpectrumTexture -- ^ kr
   -> SpectrumTexture -- ^ ks
   -> ScalarTexture -- ^ roughness
   -> Material

mkShinyMetal kr ks rough dg = mkBsdf [r, s] $ shadingCs dg where
   r = MkAnyBxdf $ Microfacet (Blinn (1 / rough dg)) frMf white
   s = MkAnyBxdf $ mkSpecRefl white frSr
   frMf = frConductor (approxEta $ ks dg) black
   frSr = frConductor (approxEta $ kr dg) black

approxEta :: Spectrum -> Spectrum
approxEta r = (white + r') / (white - r') where
   r' = sSqrt $ sClamp 0 0.999 r
