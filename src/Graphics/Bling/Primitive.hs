{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Primitive (

   -- * Ray - Primitive intersections

   Intersection(..), intLe, intLight, intBsdf,

   -- * Primitives

   Primitive(..), Geometry, mkGeom, nearest, nearest',
   AnyPrim(..), mkAnyPrim
   
   ) where

import qualified Data.Vector.Generic as V
   
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Light as L
import Graphics.Bling.Math
import Graphics.Bling.Reflection
import qualified Graphics.Bling.Shape as S
import Graphics.Bling.Spectrum

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
   shadingGeometry _ _ dgg = dgg

--
-- the existential primitive
--

data AnyPrim = forall a . Primitive a => MkAnyPrim a

instance Primitive AnyPrim where
   intersect (MkAnyPrim p) = intersect p
   {-# INLINE intersect #-}
   
   intersects (MkAnyPrim p) = intersects p
   {-# INLINE intersects #-}
   
   worldBounds (MkAnyPrim p) = worldBounds p
   {-# INLINE worldBounds #-}
   
   flatten (MkAnyPrim p) = flatten p
   {-# INLINE flatten #-}
   
   light (MkAnyPrim p) = light p
   {-# INLINE light #-}

   shadingGeometry (MkAnyPrim p) = shadingGeometry p
   {-# INLINE shadingGeometry #-}
   
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

instance Primitive Geometry where
   flatten g = [MkAnyPrim g]
   
   worldBounds g = S.worldBounds (shape g) (o2w g)
   
   intersects g rw = {-# SCC "intersects.Geometry" #-}
      S.intersects (shape g) (transRay (w2o g) rw)
   
   light g = emission g >>= \e ->
      Just $ mkAreaLight (shape g) e (o2w g) (geomId g)
   
   intersect g rw = {-# SCC "intersect.Geometry" #-}
      (S.intersect (shape g) ro) >>= int where
         int (t, dg) = Just $ Intersection t (transDg (o2w g) dg) p m
         m = material g
         p = mkAnyPrim g
         ro = transRay (w2o g) rw -- ray in object space
   
near :: (Primitive a) => (Ray, Maybe Intersection) -> a -> (Ray, Maybe Intersection)
{-# INLINE near #-}
near o@(r, _) p = maybe o go $ intersect p r where
   go i = (r { rayMax = intDist i }, Just i)

nearest :: (Primitive a, V.Vector v a) => v a -> Ray -> Maybe Intersection
{-# INLINE nearest #-}
nearest ps r = snd $ nearest' ps (r, Nothing)
   
nearest' :: (Primitive a, V.Vector v a)
   => v a
   -> (Ray, Maybe Intersection)
   -> (Ray, Maybe Intersection)
{-# INLINE nearest' #-}
nearest' ps x = V.foldl' near x ps

--
-- Intersections
--

data Intersection = Intersection {
   intDist        :: {-# UNPACK #-} ! Float,
   intGeometry    :: {-# UNPACK #-} ! DifferentialGeometry,
   intPrimitive   :: ! AnyPrim,
   intMaterial    :: ! Material
   }

intBsdf :: Intersection -> Bsdf
intBsdf int = {-# SCC "intBsdf" #-} intMaterial int dgg dgs where
   dgg = intGeometry int
   dgs = shadingGeometry (intPrimitive int) identity dgg

-- | the light emitted at an @Intersection@
intLe
   :: Intersection -- ^ the intersection to query for the emitted light
   -> Vector       -- ^ the outgoing direction (must be normalized)
   -> Spectrum     -- ^ the resulting spectrum
{-# INLINE intLe #-}
intLe (Intersection _ dg prim _) wo =
   maybe black (\l -> L.lEmit l p n wo) (light prim) where
      p = dgP dg
      n = dgN dg
   
intLight :: Intersection -> Maybe Light
{-# INLINE intLight #-}
intLight = light . intPrimitive
