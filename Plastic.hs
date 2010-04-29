
module Plastic where

import Geometry
import Material
import Math
import Microfacet
import Specular
import Texture
import Transport

data Plastic = Plastic {
   plasticDiffuse :: AnyTexture,
   plasticSpecular :: AnyTexture,
   plasticRoughness :: Float
   }
   
instance Material Plastic where
   materialBsdf (Plastic kd ks rough) dg = (Bsdf [diff, spec] sc) where
      diff = MkAnyBxdf $ Lambertian $ rd
      spec = MkAnyBxdf $ Microfacet (Blinn (1 / rough)) (frDiel 1.5 1.0) rs
      rd = evalTexture kd dg
      rs = evalTexture ks dg
      sc = coordinateSystem $ dgN dg
      