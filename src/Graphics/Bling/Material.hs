
module Graphics.Bling.Material(
   
   Material, Lambertian(..),
   
   matteMaterial, blackBodyMaterial
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture
import Graphics.Bling.Transport

type Material = DifferentialGeometry -> Bsdf

matteMaterial :: SpectrumTexture -> Material
matteMaterial tex dg = mkBsdf [bxdf] sc where
   bxdf = MkAnyBxdf $ Lambertian $ tex dg
   sc = shadingCs dg
   
blackBodyMaterial :: Material
blackBodyMaterial dg = mkBsdf [] $ shadingCs dg

data Lambertian = Lambertian {-# UNPACK #-} !Spectrum

instance Bxdf Lambertian where
   bxdfEval (Lambertian r) _ _ = sScale r invPi
   bxdfType _ = mkBxdfType [Reflection, Diffuse]
