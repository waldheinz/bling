
module Graphics.Bling.Primitive (

   -- * Ray - Primitive intersections

   Intersection, mkIntersection, intLe, intLight,
   intDist, intEpsilon, intBsdf, intGeometry,

   -- * Primitives

   Primitive(..), nearest, nearest'

   ) where

import qualified Data.Vector as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Light as L
import Graphics.Bling.Reflection

data Primitive = Primitive
   {  intersect         :: Ray -> Maybe Intersection
   ,  intersects        :: Ray -> Bool
   ,  worldBounds       :: {-# UNPACK #-} ! AABB
   ,  light             :: Maybe Light
   ,  shadingGeometry   :: DifferentialGeometry -> Transform  -> DifferentialGeometry
   }

near :: (Ray, Maybe Intersection) -> Primitive -> (Ray, Maybe Intersection)
{-# INLINE near #-}
near o@(r, _) p = maybe o go $ intersect p r where
   go i = (r { rayMax = intDist i }, Just i)

nearest :: V.Vector Primitive -> Ray -> Maybe Intersection
{-# INLINE nearest #-}
nearest ps r = snd $ nearest' ps (r, Nothing)

nearest'
   :: V.Vector Primitive
   -> (Ray, Maybe Intersection)
   -> (Ray, Maybe Intersection)
{-# INLINE nearest' #-}
nearest' ps x = V.foldl' near x ps

--
-- Intersections
--

data Intersection = Intersection
   { intDist      :: {-# UNPACK #-} ! Float
   , intEpsilon   :: {-# UNPACK #-} ! Float
   , intGeometry  :: DifferentialGeometry -- ^ true geometry at intersection point
   , intPrimitive :: ! Primitive
   , intBsdf      :: Bsdf
   }

mkIntersection
   :: Float
   -> Float
   -> DifferentialGeometry
   -> Primitive
   -> Material
   -> Intersection
mkIntersection d e dg p mat = Intersection d e dg p bsdf where
   bsdf = mat dg (shadingGeometry p dg mempty)

-- | the light emitted at an @Intersection@
intLe
   :: Intersection -- ^ intersection to query for the emitted light
   -> Vector       -- ^ outgoing direction (must be normalized)
   -> Spectrum     -- ^ emitted spectrum
{-# INLINE intLe #-}
intLe (Intersection _ _ dg prim _) wo =
   maybe black (\l -> L.lEmit l p n wo) (light prim) where
      p = dgP dg
      n = dgN dg

intLight :: Intersection -> Maybe Light
{-# INLINE intLight #-}
intLight = light . intPrimitive
