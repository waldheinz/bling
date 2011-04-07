
module Graphics.Bling.Material.Matte (
   mkMatte
   ) where

import Graphics.Bling.Reflection
import Graphics.Bling.Texture

mkMatte :: SpectrumTexture -> Material
mkMatte tex dg = mkBsdf [bxdf] sc where
   bxdf = MkAnyBxdf $ Lambertian $ tex dg
   sc = shadingCs dg