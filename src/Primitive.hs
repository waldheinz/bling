{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import AABB
import Geometry
import Light
import Material
import Math
import Shape()
import Spectrum
import Transform
import Transport

import Data.Maybe(fromJust, isJust, isNothing)

data Intersection = Intersection {
   intDist :: Float,
   intGeometry :: DifferentialGeometry,
   intPrimitive :: Primitive,
   intMaterial :: Material
   }

intBsdf :: Intersection -> Bsdf
intBsdf int = intMaterial int $ intGeometry int

-- | the light emitted at this intersection point
intLe :: Intersection -> Normal -> Spectrum
intLe (Intersection _ (DifferentialGeometry p n) prim _) wo 
   | isJust light = lightLe (fromJust light) p n wo
   | otherwise = black
   where
         light = primLight prim
   
class Prim a where
   primIntersect :: a -> Ray -> Maybe Intersection
   primIntersects :: a -> Ray -> Bool
   primWorldBounds :: a -> AABB
   primFlatten :: a -> [AnyPrim]
   
   primLight :: a -> Maybe Light
   primLight _ = Nothing
   
   -- | returns the geometry that should be used for shading computations
   shadingGeometry :: a -> Transform -> DifferentialGeometry -> DifferentialGeometry
   
   -- | the default implementation just returns the provided DG, so the
   --   geometry used for shading is the same as for reflection calculations
   shadingGeometry _ _ dg = dg
   
data AnyPrim = forall a . Prim a => MkAnyPrim a

instance Prim AnyPrim where
   primIntersect (MkAnyPrim p) = primIntersect p
   primIntersects (MkAnyPrim p) = primIntersects p
   primWorldBounds (MkAnyPrim p) = primWorldBounds p
   primFlatten (MkAnyPrim p) = primFlatten p
   primLight (MkAnyPrim p) = primLight p

data Primitive
   = Geometric (Ray -> Maybe (Float, DifferentialGeometry)) (Ray -> Bool) Material (Maybe Light) AABB -- ^ a bound primitive
   | Group [AnyPrim]

instance Prim Primitive where
   primIntersect (Group []) _ = Nothing
   primIntersect (Group g) r = nearest r g
   primIntersect p@(Geometric i _ m _ _) r
      | isJust (i r) = Just $ let (t, dg) = fromJust (i r) in Intersection t dg p m
      | otherwise = Nothing

   primWorldBounds (Geometric _ _ _ _ b) = b
   primWorldBounds (Group g) = foldl extendAABB emptyAABB $ map primWorldBounds g
   
   primIntersects (Group []) _ = False
   primIntersects (Group (x:xs)) r = primIntersects x r || primIntersects (Group xs) r
   primIntersects (Geometric _ i _ _ _) r = i r
   
   primFlatten g@(Geometric _ _ _ _ _) = [MkAnyPrim g]
   primFlatten (Group (p:xs)) = primFlatten p ++ concatMap primFlatten xs
   primFlatten (Group []) = []
   
   primLight (Geometric _ _ _ l _) = l
   primLight (Group _) = error "primLight for a group requested"
   
-- | creates a @Primitive@ for the specified @Bound@, @Material@ and possibly @Spectrum@ for light sources
mkPrim :: (Geometry b) => b -> Material -> Maybe Spectrum -> Primitive
mkPrim b mat Nothing = Geometric (intersect b) (intersects b) mat Nothing (worldBounds b)
mkPrim b mat (Just l) = Geometric (intersect b) (intersects b) mat (Just (mkAreaLight l b)) (worldBounds b)

nearest :: (Prim a) => Ray -> [a] -> Maybe Intersection
nearest (Ray ro rd tmin tmax) i = nearest' i tmax Nothing where
   nearest' [] _ mi = mi
   nearest' (x:xs) tmax' mi = nearest' xs newMax newNear where
      clamped = Ray ro rd tmin tmax'
      newNear = if isJust newNear' then newNear' else mi
      newNear' = primIntersect x clamped
      newMax = if isNothing newNear
                  then tmax'
                  else intDist $ fromJust newNear
