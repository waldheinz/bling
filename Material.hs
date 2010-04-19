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
   
data Measured = BrushedMetal | BluePaint

instance Material Measured where
   materialBsdf BluePaint int = (Bsdf bxdf sc) where
      sc = coordinates int
      bxdf = MkAnyBxdf $ Lafortune (0.3094, 0.39667, 0.70837) lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = (0.870567,   0.857255, 0.670982)
      z1 =  (0.803624,   0.774290, 0.586674)
      e1 =  (21.820103, 18.597755, 7.472717)
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = (-0.451218, -0.406681, -0.477976)
      z2 =  (0.023123, 0.017625, 0.227295)
      e2 =  (2.774499, 2.581499, 3.677653)
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = (-1.031545, -1.029426, -1.026588)
      z3 =  (0.706734, 0.696530, 0.687715)
      e3 =  (66.899060, 63.767912, 57.489181)
   
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
      
      