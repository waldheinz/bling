module Graphics.Bling.Primitive.Geometry (
   Geometry, mkGeom
   ) where

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Light
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import qualified Graphics.Bling.Shape as S

data Geometry = MkGeometry {
   geomId   :: {-# UNPACK #-} ! Int,
   o2w      :: {-# UNPACK #-} ! Transform, -- ^ the object-to-world transformation
   w2o      :: {-# UNPACK #-} ! Transform, -- ^ the world-to-object transformation
   _ro      :: ! Bool, -- ^ reverse the normal orientation?
   shape    :: ! S.Shape,
   material :: ! Material,
   emission :: ! (Maybe Spectrum)
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
      shape g `S.intersect` ro >>= int where
         int (t, e, dg) = Just $ mkIntersection t e (transDg (o2w g) dg) p m
         m = material g
         p = mkAnyPrim g
         ro = transRay (w2o g) rw -- ray in object space
