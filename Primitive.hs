{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import Geometry
import Material
import Math
import Transport

import Maybe(fromJust, isJust, isNothing)

data Intersection = Intersection {
   intDist :: Float,
   intGeometry :: DifferentialGeometry,
   intPrimitive :: AnyPrimitive
   }

intBsdf :: Intersection -> Bsdf
intBsdf int = materialBsdf mat dg where
   mat = primMaterial prim
   prim = intPrimitive int
   dg = intGeometry int

class Primitive a where
   primIntersect :: a -> Ray -> Maybe Intersection
   primIntersects :: a -> Ray -> Bool
   primMaterial :: a -> AnyMaterial
   
   primMaterial _ = (MkAnyMaterial (Matte (1.0, 1.0, 0)))
   
gP :: (Intersectable i, Material m) => i -> m -> AnyPrimitive
gP i m = MkAnyPrimitive $ GeometricPrimitive (MkAnyIntersectable i) (MkAnyMaterial m)

data GeometricPrimitive = GeometricPrimitive AnyIntersectable AnyMaterial

instance Primitive GeometricPrimitive where
   primIntersect p@(GeometricPrimitive i _) ray = pi' (intersect ray i) where
      pi' :: Maybe (Float, DifferentialGeometry) -> Maybe Intersection
      pi' Nothing = Nothing
      pi' (Just (t, dg)) = Just $ Intersection t dg (MkAnyPrimitive p)
      
   primIntersects (GeometricPrimitive i _) r = intersects r i
   primMaterial (GeometricPrimitive _ m) = m
   
data Group = Group [AnyPrimitive]

nearest :: (Primitive i) => Ray -> [i] -> Maybe Intersection
nearest (Ray ro rd tmin tmax) i = nearest' i tmax Nothing where
   nearest' [] _ mi = mi
   nearest' (x:xs) tmax' mi = nearest' xs newMax newNear where
      clamped = (Ray ro rd tmin tmax')
      newNear = if (isJust newNear') then newNear' else mi
      newNear' = primIntersect x clamped
      newMax = if (isNothing newNear)
                  then tmax'
                  else intDist $ fromJust newNear

instance Primitive Group where
   primIntersect (Group []) _ = Nothing
   primIntersect (Group g) r = nearest r g
   
   primIntersects (Group []) _ = False
   primIntersects (Group (x:xs)) r = primIntersects x r || primIntersects (Group xs) r
   
data AnyPrimitive = forall a. Primitive a => MkAnyPrimitive a

instance Primitive AnyPrimitive where
   primIntersect (MkAnyPrimitive a) r = primIntersect a r
   primIntersects (MkAnyPrimitive a) r  = primIntersects a r
   primMaterial (MkAnyPrimitive a) = primMaterial a

