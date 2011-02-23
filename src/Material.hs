
module Material(Material, Lambertian(..),
   matteMaterial, blackBodyMaterial) where

import Geometry
import Math
import Spectrum
import Texture
import Transport

type Material = DifferentialGeometry -> Bsdf



matteMaterial :: SpectrumTexture -> Material
matteMaterial tex dg = mkBsdf [bxdf] sc where
   bxdf = MkAnyBxdf $ Lambertian $ tex dg
   sc = shadingCs dg
   
blackBodyMaterial :: Material
blackBodyMaterial dg = mkBsdf [] $ shadingCs dg

data Lambertian = Lambertian Spectrum

instance Bxdf Lambertian where
   bxdfEval (Lambertian r) _ _ = sScale r invPi
   bxdfType _ = mkBxdfType [Reflection, Diffuse]
     