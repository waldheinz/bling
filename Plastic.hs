
module Plastic where

import Geometry
import Material
import Math
import Specular
import Texture
import Transport

data Plastic = Plastic {
   plasticTexture :: AnyTexture
   }

instance Material Plastic where
   materialBsdf (Plastic tex) dg = (Bsdf [diff, spec] sc) where
      diff = MkAnyBxdf $ Lambertian $ r
      spec = MkAnyBxdf $ SpecularReflection r (frDiel 1.5 1.0)
      r = evalTexture tex dg
      sc = coordinateSystem $ dgN dg
      