{-# LANGUAGE ExistentialQuantification #-}

module Material(Material(..), Measured(..), AnyMaterial(..), Matte(..)) where

import Color
import Geometry
import Lafortune
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
   
data Measured = BrushedMetal

instance Material Measured where
   materialBsdf BrushedMetal int = (Bsdf bxdf sc) where
      sc = coordinates int
      bxdf = MkAnyBxdf $ Lafortune black lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = (  -1.11854, -1.11845, -1.11999 )
      z1 =  (   1.01272,  1.01469,  1.01942 )
      e1 =  (  15.8708,  15.6489,  15.4571 )
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = (  -1.05334, -1.06409, -1.08378 )
      z2 =  (   0.69541,  0.662178, 0.626672 )
      e2 =  ( 111.267,   88.9222,  65.2179 )
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = (  -1.01684,  -1.01635, -1.01529 )
      z3 =  (   1.00132,   1.00112,  1.00108 )
      e3 =  ( 180.181,   184.152,  195.773 )
      
      