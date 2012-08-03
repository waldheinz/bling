
module Graphics.Bling.Material.Metal (

   -- * Creating Metal Materials
   
   mkMetal, mkShinyMetal

   ) where


import Graphics.Bling.Texture
import Graphics.Bling.Reflection

-- | Creates a Metal @Material@
mkMetal
   :: SpectrumTexture -- ^ eta
   -> SpectrumTexture -- ^ k
   -> ScalarTexture -- ^ roughness
   -> Material

mkMetal eta k rough dgg dgs = mkBsdf' [spec] dgg dgs where
   fr = frConductor (eta dgs) (k dgs)
   spec = mkMicrofacet (mkBlinn (1 / rough dgs)) fr white

mkShinyMetal
   :: SpectrumTexture -- ^ kr
   -> SpectrumTexture -- ^ ks
   -> ScalarTexture -- ^ roughness
   -> Material

mkShinyMetal kr ks rough dgg dgs = mkBsdf' [r, s] dgg dgs where
   r = mkMicrofacet (mkBlinn (1 / rough dgs)) frMf white
   s = mkSpecularReflection white frSr
   frMf = frConductor (approxEta $ ks dgs) black
   frSr = frConductor (approxEta $ kr dgs) black

approxEta :: Spectrum -> Spectrum
approxEta r = (white + r') / (white - r') where
   r' = sSqrt $ sClamp 0 0.999 r
