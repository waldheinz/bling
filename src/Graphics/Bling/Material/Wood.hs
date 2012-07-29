
module Graphics.Bling.Material.Wood (
   mkWood, woodTexture
   ) where

import Graphics.Bling.Reflection
import Graphics.Bling.Texture

import Data.Monoid

mkWood :: Material
mkWood dgg dgs = mkBsdf' [diff, spec] dgg dgBump where
   spec = MkAnyBxdf $ Microfacet (mkBlinn (1 / 0.01)) (frDielectric 1 1.5) white
   
   diff = MkAnyBxdf $ Lambertian clouds2
   
   s = scale $ mkV (5, 5, 5)
   o = scale $ mkV (1, 1, 0.05)
   d = addTexture
      (scaleTexture 3 (fbmTexture 6 0.46 (identityMapping3d mapping)))
      (rings)
   
   dgBump = bump (scaleTexture (-0.005) d) dgg dgs
   
   mapping = o <> s
   clouds2 = gradient g2 d dgs
   g2 = mkGradient [(-1, clouds1), (0.10, clouds1), (0.30, c2), (1, c2)]
   m = identityMapping3d $ (scale $ mkV (0.5, 0.5, 0.5)) <> s
   clouds1 = gradient g1 (fbmTexture 6 0.8 m) dgg
   g1 = mkGradient [(-1, c0), (-0.15, c0), (0.3, c1)]
   
   c0 = fromRGB (0.376, 0.263, 0.169)
   c1 = fromRGB (0.651, 0.447, 0.290)
   c2 = fromRGB (0.269, 0.219, 0.198)
   
   rings :: ScalarTexture
   rings dg = sin (sqrt (x*x + y*y + z*z) * 10 + (scaleTexture 3 (noiseTexture (identityMapping3d mapping)) dg)) where
      (x, y, z) = identityMapping3d mapping dg

--
-- Wood
--

woodTexture :: SpectrumTexture
woodTexture dg = clouds2 where
   s = scale $ mkV (1, 1, 1)
   o = scale $ mkV (20, 1, 1)
   mapping = o <> s
   clouds2 = gradient g2 (fbmTexture 6 0.46 (identityMapping3d mapping)) dg
   g2 = mkGradient [(-1, clouds1), (0.15, clouds1), (0.20, c2), (1, c2)]

   clouds1 = gradient g1 (fbmTexture 6 0.8 $ identityMapping3d $ (scale $ mkV (0.5, 0.5, 0.5)) <> s) dg
   g1 = mkGradient [(-1, c0), (-0.15, c0), (0.3, c1)]

   c0 = fromRGB (0.376, 0.263, 0.169)
   c1 = fromRGB (0.651, 0.447, 0.290)
   c2 = fromRGB (0.269, 0.219, 0.198)
