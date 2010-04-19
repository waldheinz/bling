{-# LANGUAGE ExistentialQuantification #-}

module Material(Material(..), AnyMaterial(..), Matte(..)) where

import Geometry
import Math
import Transport

class Material a where
   materialBsdf :: a -> DifferentialGeometry -> Bsdf

data AnyMaterial = forall a. Material a => MkAnyMaterial a

instance Material AnyMaterial where
   materialBsdf (MkAnyMaterial a) = materialBsdf a

data Matte = Matte Spectrum

instance Material Matte where
   materialBsdf (Matte r) int = (Bsdf bxdf sc) where
      bxdf = MkAnyBxdf $ Lambertian $ r
      sc = coordinates int
   
data Lambertian = Lambertian Spectrum
   
instance Bxdf Lambertian where
   bxdfEval (Lambertian r) _ _ = scalMul r invPi
   bxdfType _ = Reflection
   bxdfAppearance _ = Diffuse

coordinates :: DifferentialGeometry -> LocalCoordinates
coordinates dg = (LocalCoordinates sn tn n) where
   (sn, tn) = coordinateSystem n
   n = dgN dg
   