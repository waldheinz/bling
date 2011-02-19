{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import AABB
import Geometry
import Light
import Material
import Math
import Spectrum
import Transport

import Maybe(fromJust, isJust, isNothing)

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
   
data AnyPrim = forall a . Prim a => MkAnyPrim a

instance Prim AnyPrim where
   primIntersect (MkAnyPrim p) = primIntersect p
   primIntersects (MkAnyPrim p) = primIntersects p
   primWorldBounds (MkAnyPrim p) = primWorldBounds p
   
data Primitive
   = GeometricU (Ray -> Maybe (Float, DifferentialGeometry)) (Ray -> Bool) Material -- ^ an unbound primitive
   | GeometricB (Ray -> Maybe (Float, DifferentialGeometry)) (Ray -> Bool) Material (Maybe Light) AABB -- ^ a bound primitive
   | Group [Primitive]
   
instance Show Primitive where
   show (Group ps) = "Group [" ++ (concat $ map show ps) ++ "]"
   show (GeometricU _ _ _) = "<unbound>"
   show (GeometricB _ _ _ _ _) = "<bound>"
   
instance Prim Primitive where
   primIntersect (Group []) _ = Nothing
   primIntersect (Group g) r = nearest r g
   primIntersect p@(GeometricU i _ m) r
      | isJust (i r) = Just $ let (t, dg) = fromJust (i r) in Intersection t dg p m
      | otherwise = Nothing
   primIntersect p@(GeometricB i _ m _ _) r
      | isJust (i r) = Just $ let (t, dg) = fromJust (i r) in Intersection t dg p m
      | otherwise = Nothing
   
   primWorldBounds (GeometricB _ _ _ _ b) = b
   primWorldBounds (GeometricU _ _ b) = error "no bounds"
   
--    primWorldBounds 
   
   primIntersects (Group []) _ = False
   primIntersects (Group (x:xs)) r = primIntersects x r || primIntersects (Group xs) r
   primIntersects (GeometricU _ i _) r = i r
   primIntersects (GeometricB _ i _ _ _) r = i r
   
-- | creates a @Primitive@ for the specified @Intersectable@ and @Material@
mkPrim :: (Intersectable i) => i -> Material -> Primitive
mkPrim int mat = GeometricU (intersect int) (intersects int) mat

-- | creates a @Primitive@ for the specified @Bound@, @Material@ and possibly @Spectrum@ for light sources
mkPrim' :: (Bound b) => b -> Material -> Maybe Spectrum -> Primitive
mkPrim' b mat Nothing = GeometricB (intersect b) (intersects b) mat Nothing (boundAABB b)
mkPrim' b mat (Just l) = GeometricB (intersect b) (intersects b) mat (Just $ mkAreaLight l b) (boundAABB b)

isBound :: Primitive -> Bool
isBound (Group ps) = all isBound ps
isBound (GeometricU _ _ _) = False
isBound (GeometricB _ _ _ _ _) = True

primBounds :: Primitive -> AABB
primBounds (Group g) = foldl extendAABB emptyAABB $ map primBounds g
primBounds (GeometricB _ _ _ _ aabb) = aabb
primBounds (GeometricU _ _ _) = error "bounds for an unbound requested"

primFlatten :: Primitive -> [Primitive]
primFlatten (Group (p:xs)) = (primFlatten p) ++ (concat $ map primFlatten xs)
primFlatten p = [p]

primLight :: Primitive -> Maybe Light
primLight (GeometricB _ _ _ l _) = l
primLight _ = Nothing

nearest :: (Prim a) => Ray -> [a] -> Maybe Intersection
nearest (Ray ro rd tmin tmax) i = nearest' i tmax Nothing where
   nearest' [] _ mi = mi
   nearest' (x:xs) tmax' mi = nearest' xs newMax newNear where
      clamped = (Ray ro rd tmin tmax')
      newNear = if (isJust newNear') then newNear' else mi
      newNear' = primIntersect x clamped
      newMax = if (isNothing newNear)
                  then tmax'
                  else intDist $ fromJust newNear
