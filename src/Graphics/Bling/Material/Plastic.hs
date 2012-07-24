
module Graphics.Bling.Material.Plastic (
   -- * Creating Plastic Material
   mkPlastic
   
   ) where

import Graphics.Bling.Texture
import Graphics.Bling.Reflection

mkPlastic
   :: SpectrumTexture -- ^ diffuse spectrum
   -> SpectrumTexture -- ^ specular spectrum
   -> ScalarTexture -- ^ roughness
   -> Material
   
mkPlastic kd ks kr dgg dgs = mkBsdf' [diff, spec] dgg dgs where
   diff = MkAnyBxdf $ Lambertian rd
   spec = MkAnyBxdf $ Microfacet (mkBlinn (1 / rough)) (frDielectric 1.0 1.5) rs
   rough = kr dgs
   rd = kd dgs
   rs = ks dgs
   
