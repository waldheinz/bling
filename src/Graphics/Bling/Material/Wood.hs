
module Graphics.Bling.Material.Wood (
   woodTexture
   ) where

import Graphics.Bling.Spectrum
import Graphics.Bling.Texture
import Graphics.Bling.Transform

--
-- Wood
--

woodTexture :: SpectrumTexture
woodTexture dg = clouds2 where
   mapping = identityMapping3d $ scale $ mkV (20, 1, 1)
   clouds2 = gradient g2 (fbmTexture 6 0.46 mapping) dg
   g2 = mkGradient [(0, clouds1), (0.15, clouds1), (0.20, c2), (1, c2)]

   clouds1 = gradient g1 (fbmTexture 6 0.8 $ identityMapping3d identity) dg
   g1 = mkGradient [(0, c0), (0.3, c0), (1, c1)]

   c0 = fromRGB (0.376, 0.263, 0.169)
   c1 = fromRGB (0.651, 0.447, 0.290)
   c2 = fromRGB (0.269, 0.219, 0.198)
