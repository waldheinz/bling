{-# LANGUAGE ExistentialQuantification #-}

module Material(Material(..), Matte(..), defaultMaterial) where

import Bsdf
import Geometry
import Math
import Transport

defaultMaterial :: Matte
defaultMaterial = Matte (0.85, 0.85, 0.85)

class Material a where
   materialBsdf :: a -> Intersection -> Bsdf

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

coordinates :: Intersection -> LocalCoordinates
coordinates (Intersection _ _ n) = (LocalCoordinates (normalize sn) (normalize tn) (normalize n)) where
   (sn', tn') = coordinateSystem n
   (sn, tn) = if (sn' `dot` n < 0)
                 then (sn', tn')
                 else (tn', sn')
