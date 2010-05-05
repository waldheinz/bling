
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
   = GeometricU (Ray -> Maybe (Float, DifferentialGeometry)) (Ray -> Bool) Material -- ^ an unbound primitive
   | GeometricB (Ray -> Maybe (Float, DifferentialGeometry)) (Ray -> Bool) Material (Maybe Light) AABB -- ^ a bound primitive
   | Group [Primitive]
   
-- | creates a @Primitive@ for the specified @Intersectable@ and @Material@
mkPrim :: (Intersectable i) => i -> Material -> Primitive
mkPrim int mat = GeometricU (intersect int) (intersects int) mat

-- | creates a @Primitive@ for the specified @Bound@, @Material@ and possibly @Spectrum@ for light sources
mkPrim' :: (Bound b) => b -> Material -> Maybe Spectrum -> Primitive
mkPrim' b mat Nothing = GeometricB (intersect b) (intersects b) mat Nothing (boundAABB b)
mkPrim' b mat (Just l) = GeometricB (intersect b) (intersects b) mat (Just $ mkAreaLight l b) (boundAABB b)

primBounds :: Primitive -> AABB
primBounds (Group g) = foldl extendAABB emptyAABB $ map primBounds g
primBounds (GeometricB _ _ _ _ aabb) = aabb
primBounds (GeometricU _ _ _) = error "bounds for an unbound requested"

primFlatten :: Primitive -> [Primitive]
primFlatten (Group (p:xs)) = (primFlatten p) ++ (concat $ map primFlatten xs)
primFlatten p = [p]

primIntersect :: Primitive -> Ray -> Maybe Intersection
primIntersect (Group []) _ = Nothing
primIntersect (Group g) r = nearest r g
primIntersect p@(GeometricU int _ _) ray = hit2Int p (int ray)
primIntersect p@(GeometricB int _ _ _ _) ray = hit2Int p (int ray)

hit2Int :: Primitive -> Maybe (Float, DifferentialGeometry) -> Maybe Intersection
hit2Int _ Nothing = Nothing
hit2Int p (Just (t, dg)) = Just $ Intersection t dg p

primIntersects :: Primitive -> Ray -> Bool
primIntersects (Group []) _ = False
primIntersects (Group (x:xs)) r = primIntersects x r || primIntersects (Group xs) r
primIntersects (GeometricU _ i _) r = i r
primIntersects (GeometricB _ i _ _ _) r = i r

primMaterial :: Primitive -> Material
primMaterial (GeometricU _ _ mat) = mat
primMaterial (GeometricB _ _ mat _ _) = mat
primMaterial _ = undefined

primLight :: Primitive -> Maybe Light
primLight (GeometricU _ _ _) = Nothing
primLight (GeometricB _ _ _ l _) = l
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
