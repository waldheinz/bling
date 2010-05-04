
module Primitive where

import Color
import Geometry
import Light
import Material
import Math
import Transport

import Maybe(fromJust, isJust, isNothing)

data Intersection = Intersection {
   intDist :: Float,
   intGeometry :: DifferentialGeometry,
   intPrimitive :: Primitive
   }

intBsdf :: Intersection -> Bsdf
intBsdf int = mat dg where
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
   
data Primitive
   = GeometricPrimitive (Ray -> Maybe (Float, DifferentialGeometry)) (Ray -> Bool) Material (Maybe Light)
   | Group [Primitive]
   
mkGeometricPrimitive :: (Intersectable i) => i -> Material -> (Maybe Light) -> Primitive
mkGeometricPrimitive int mat ml =
   GeometricPrimitive (intersect int) (intersects int) mat ml

primIntersect :: Primitive -> Ray -> Maybe Intersection
primIntersect (Group []) _ = Nothing
primIntersect (Group g) r = nearest r g
primIntersect p@(GeometricPrimitive int _ _ _) ray = pi' (int ray) where
      pi' :: Maybe (Float, DifferentialGeometry) -> Maybe Intersection
      pi' Nothing = Nothing
      pi' (Just (t, dg)) = Just $ Intersection t dg p
   

primIntersects :: Primitive -> Ray -> Bool
primIntersects (Group []) _ = False
primIntersects (Group (x:xs)) r = primIntersects x r || primIntersects (Group xs) r
primIntersects (GeometricPrimitive _ i _ _) r = i r

primMaterial :: Primitive -> Material
primMaterial (GeometricPrimitive _ _ mat _) = mat
primMaterial _ = undefined

primLight :: Primitive -> Maybe Light
primLight (GeometricPrimitive _ _ _ l) = l
primLight _ = Nothing

nearest :: Ray -> [Primitive] -> Maybe Intersection
nearest (Ray ro rd tmin tmax) i = nearest' i tmax Nothing where
   nearest' [] _ mi = mi
   nearest' (x:xs) tmax' mi = nearest' xs newMax newNear where
      clamped = (Ray ro rd tmin tmax')
      newNear = if (isJust newNear') then newNear' else mi
      newNear' = primIntersect x clamped
      newMax = if (isNothing newNear)
                  then tmax'
                  else intDist $ fromJust newNear
