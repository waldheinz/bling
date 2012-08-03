

module Graphics.Bling.Material.Substrate (
   -- * Creating Substrate Material
   mkSubstrate

   ) where

import Graphics.Bling.Texture
import Graphics.Bling.Reflection

mkSubstrate
   :: SpectrumTexture
   -> SpectrumTexture
   -> ScalarTexture
   -> ScalarTexture
   -> Material

mkSubstrate kd ks tur tvr dgg dgs = mkBsdf' [brdf] dgg dgs where
   brdf = mkFresnelBlend rd rs dist
   dist = mkAnisotropic (1 / u) (1 / v)
   u = tur dgs
   v = tvr dgs
   rd = sClamp 0 1 $ kd dgs
   rs = sClamp 0 1 $ ks dgs
