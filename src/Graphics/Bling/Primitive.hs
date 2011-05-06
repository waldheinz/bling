{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Primitive (

   -- * Ray - Primitive intersections

   Intersection(..), intLe, intLight, intBsdf,

   -- * Primitives

   Primitive(..), Geometry, mkGeom, mkMesh, nearest, AnyPrim(..), mkAnyPrim
   
   ) where

import Graphics.Bling.AABB
import Graphics.Bling.Light as L
import Graphics.Bling.Math
import Graphics.Bling.Reflection
import qualified Graphics.Bling.Shape as S
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

import Data.Maybe(fromJust, isJust, isNothing)

class Primitive a where
   intersect :: a -> Ray -> Maybe Intersection
   intersects :: a -> Ray -> Bool
   worldBounds :: a -> AABB
   flatten :: a -> [AnyPrim]
   
   light :: a -> Maybe Light
   
   -- | The default implementation returns @Nothing@.
   light _ = Nothing
   
   -- | returns the geometry that should be used for shading computations
   shadingGeometry :: a -> Transform -> DifferentialGeometry -> DifferentialGeometry
   
   -- | the default implementation just returns the provided DG, so the
   --   geometry used for shading is the same as for reflection calculations
   shadingGeometry _ _ dg = dg

--
-- the existential primitive
--

data AnyPrim = forall a . Primitive a => MkAnyPrim a

instance Primitive AnyPrim where
   intersect (MkAnyPrim p) = intersect p
   intersects (MkAnyPrim p) = intersects p
   worldBounds (MkAnyPrim p) = worldBounds p
   flatten (MkAnyPrim p) = flatten p
   light (MkAnyPrim p) = light p

mkAnyPrim :: (Primitive a) => a -> AnyPrim
mkAnyPrim = MkAnyPrim

--
-- geometric primitives
--   

data Geometry = MkGeometry {
   geomId :: Int,
   o2w :: Transform, -- ^ the object-to-world transformation
   w2o :: Transform, -- ^ the world-to-object transformation
   _reverseOrientation :: Bool, -- ^ reverse the normal orientation?
   shape :: S.Shape,
   material :: Material,
   emission :: Maybe Spectrum
   } 

mkGeom
   :: Transform
   -> Bool
   -> Material
   -> Maybe Spectrum
   -> S.Shape
   -> Int
   -> Geometry
mkGeom t ro m e s gid = MkGeometry gid t (inverse t) ro s m e

mkMesh
   :: Material
   -> Maybe Spectrum
   -> Transform
   -> [[S.Vertex]]
   -> Int -- ^ geometry id
   -> [Geometry]
mkMesh m e t vs gid = map (\s -> mkGeom t False m e s gid) (S.triangulate vs)

instance Eq Geometry where

-- | transforms a @DifferentialGeometry@ to world space
transDg :: Transform -> DifferentialGeometry -> DifferentialGeometry
{-# INLINE transDg #-}
transDg t (DifferentialGeometry p n) =
   DifferentialGeometry (transPoint t p) (transNormal t n)

instance Primitive Geometry where
   flatten g = [MkAnyPrim g]
   
   worldBounds g = S.worldBounds (shape g) (o2w g)
   
   intersects g rw = S.intersects (shape g) (transRay (w2o g) rw)
   
   light g = emission g >>= \e ->
      Just $ mkAreaLight (shape g) e (o2w g) (geomId g)
   
   intersect g rw = (S.intersect (shape g) ro) >>= int where
      int (t, dg) = Just $ Intersection t (transDg (o2w g) dg) p m
      m = material g
      p = MkAnyPrim g
      ro = transRay (w2o g) rw -- ray in object space
   
nearest :: (Primitive a) => Ray -> [a] -> Maybe Intersection
{-# INLINE nearest #-}
nearest (Ray ro rd tmin tmax) i = nearest' i tmax Nothing where
   nearest' [] _ mi = mi
   nearest' (x:xs) tmax' mi = nearest' xs newMax newNear where
      clamped = Ray ro rd tmin tmax'
      newNear = if isJust newNear' then newNear' else mi
      newNear' = intersect x clamped
      newMax = if isNothing newNear
                  then tmax'
                  else intDist $ fromJust newNear

--
-- Intersections TODO: strictness here? :TODO
--

data Intersection = Intersection {
   intDist :: Float,
   intGeometry :: DifferentialGeometry,
   intPrimitive :: AnyPrim,
   intMaterial :: Material
   }

intBsdf :: Intersection -> Bsdf
intBsdf int = intMaterial int $ intGeometry int

-- | the light emitted at an @Intersection@
intLe
   :: Intersection -- ^ the intersection to query for the emitted light
   -> Normal -- ^ the outgoing direction
   -> Spectrum -- ^ the resulting spectrum
{-# INLINE intLe #-}
intLe (Intersection _ (DifferentialGeometry p n) prim _) wo =
   maybe black (\l -> L.lEmit l p n wo) (light prim)
   
intLight :: Intersection -> Maybe Light
{-# INLINE intLight #-}
intLight = light . intPrimitive
