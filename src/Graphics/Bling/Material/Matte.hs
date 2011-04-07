
module Graphics.Bling.Material.Matte (

   -- * Creating a Matte Material
   mkMatte
   
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Reflection
import Graphics.Bling.Texture

-- | Creates a matte @Material@, which uses either a @Lambertian@ or
--   an @OrenNayar@ BxDF, depending on the roughness at the intersection
mkMatte
   :: SpectrumTexture -- ^ the overall color
   -> Texture Flt -- ^ the sigma (roughness) parameter
   -> Material
   
mkMatte tex ts dg
   | s == 0 = mkBsdf [MkAnyBxdf $ Lambertian r] sc
   | otherwise = mkBsdf [MkAnyBxdf $ mkOrenNayar r s] sc
   where
      sc = shadingCs dg
      s = ts dg
      r = tex dg
