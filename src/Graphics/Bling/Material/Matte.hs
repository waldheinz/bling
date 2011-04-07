
module Graphics.Bling.Material.Matte (
   mkMatte
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Reflection
import Graphics.Bling.Texture

-- | creates a Matte material
mkMatte
   :: SpectrumTexture
   -> Texture Flt
   -> Material
   
mkMatte tex ts dg
   | s == 0 = mkBsdf [MkAnyBxdf $ Lambertian r] sc
   | otherwise = mkBsdf [MkAnyBxdf $ mkOrenNayar r s] sc
   where
      sc = shadingCs dg
      s = ts dg
      r = tex dg
   