
module Graphics.Bling.Primitive.Geometry (

   mkGeom
   
   ) where

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Light
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import qualified Graphics.Bling.Shape as S

mkGeom
   :: Transform
   -> Bool
   -> Material
   -> Maybe Spectrum
   -> S.Shape
   -> Int
   -> Primitive
mkGeom o2w _ m e s gid = prim where
   prim = Primitive inter inters wb lig const
   w2o = inverse o2w
   wb = S.worldBounds s o2w
   inters rw = {-# SCC "intersects" #-}
      s `S.intersects` (transRay w2o rw)
   
   lig = case e of
      Nothing  -> Nothing
      Just r   -> Just $ mkAreaLight s r o2w gid
   
   inter rw = {-# SCC "intersect" #-}
      s `S.intersect` ro >>= int where
         int (t, eps, dg) = Just $ mkIntersection t eps (transDg o2w dg) prim m
         ro = transRay w2o rw -- ray in object space
         
