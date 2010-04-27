{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import Geometry
import Light
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

-- | the light emitted at this intersection point
intLe :: Intersection -> Normal -> Spectrum
intLe (Intersection _ (DifferentialGeometry p n) prim) wo 
   | isJust light = lightLe (fromJust light) p n wo
   | otherwise = black
   where
         light = primLight prim
   
class Primitive a where
   primIntersect :: a -> Ray -> Maybe Intersection
   primIntersects :: a -> Ray -> Bool
   primMaterial :: a -> AnyMaterial
   primLight :: a -> Maybe Light
   
   primMaterial _ = undefined
   primLight _ = Nothing
   
gP :: (Intersectable i, Material m) => i -> m -> Maybe Light -> AnyPrimitive
gP i m l = MkAnyPrimitive $ GeometricPrimitive (MkAnyIntersectable i) (MkAnyMaterial m) l

data GeometricPrimitive = GeometricPrimitive AnyIntersectable AnyMaterial (Maybe Light)

instance Primitive GeometricPrimitive where
   primIntersect p@(GeometricPrimitive i _ _) ray = pi' (intersect ray i) where
      pi' :: Maybe (Float, DifferentialGeometry) -> Maybe Intersection
      pi' Nothing = Nothing
      pi' (Just (t, dg)) = Just $ Intersection t dg (MkAnyPrimitive p)
      
   primIntersects (GeometricPrimitive i _ _) r = intersects r i
   primMaterial (GeometricPrimitive _ m _) = m
   primLight (GeometricPrimitive _ _ l) = l
   
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
   primLight (MkAnyPrimitive a) = primLight a
