{-# LANGUAGE ExistentialQuantification #-}

module Material(Material(..), Matte(..), defaultMaterial) where

import Bsdf
import Geometry
import Math
import Transport

defaultMaterial :: Matte
defaultMaterial = Matte (0.75, 0.75, 0.75)

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
   bxdfType _ = Diffuse

coordinates :: Intersection -> LocalCoordinates
coordinates (Intersection _ _ n) = (LocalCoordinates sn tn n) where
   (sn, tn) = coordinateSystem n
