
module Graphics.Bling.Material.Matte (

   -- * Creating a Matte Material
   mkMatte
   
   ) where

import Graphics.Bling.Reflection
import Graphics.Bling.Texture

-- | Creates a matte @Material@, which uses either a @Lambertian@ or
--   an @OrenNayar@ BxDF, depending on the roughness at the intersection
mkMatte
   :: SpectrumTexture -- ^ the overall color
   -> ScalarTexture -- ^ the sigma (roughness) parameter
   -> Material
   
mkMatte tex ts dgg dgs
   | s == 0 = mkBsdf' [MkAnyBxdf $ Lambertian r] dgg dgs
   | otherwise = mkBsdf' [MkAnyBxdf $ mkOrenNayar r s] dgg dgs
   where
      s = ts dgs
      r = tex dgs