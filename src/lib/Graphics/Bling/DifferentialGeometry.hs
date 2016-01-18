

module Graphics.Bling.DifferentialGeometry (
   module Graphics.Bling.Transform,

   -- * Differential Geometry

   DifferentialGeometry, mkDg, mkDg',
      dgP, dgN, dgU, dgV, dgDPDU, dgDPDV, dgDNDU, dgDNDV,
      transDg,

   -- ** special cased for triangles

   mkDgTri, dgtriB

   ) where

import Control.DeepSeq

import Graphics.Bling.Transform

--
-- Differential Geometry
--

data DifferentialGeometry = DG {
   dgP      :: {-# UNPACK #-} ! Point,
   dgN      :: {-# UNPACK #-} ! Normal,
   dgU      :: {-# UNPACK #-} ! Float,
   dgV      :: {-# UNPACK #-} ! Float,
   dgDPDU   :: {-# UNPACK #-} ! Vector,
   dgDPDV   :: {-# UNPACK #-} ! Vector,
   dgDNDU   :: {-# UNPACK #-} ! Vector,
   dgDNDV   :: {-# UNPACK #-} ! Vector,
   dgtriB   :: ! (Maybe (Float, Float)) -- barycentric coordinates for triangle hits
   } deriving (Show)

instance NFData DifferentialGeometry where
    rnf x = seq x ()

mkDg
   :: Point -- ^ intersection point
   -> Float -- ^ u parameter
   -> Float -- ^ v parameter
   -> Vector -- ^ dpdu
   -> Vector -- ^ dpdv
   -> Vector -- ^ dndu
   -> Vector -- ^ dndv
   -> DifferentialGeometry
mkDg p u v dpdu dpdv dndu dndv = DG p n u v dpdu dpdv dndu dndv Nothing where
   n = normalize $ dpdu `cross` dpdv

mkDg' :: Point -> Normal -> DifferentialGeometry
mkDg' p n = DG p n 0 0 dpdu dpdv dn dn Nothing where
   (LocalCoordinates dpdu dpdv _) = coordinateSystem n
   dn = mkV (0, 0, 0)

mkDgTri
   :: Point -- ^ intersection point
   -> Float -- ^ u parameter
   -> Float -- ^ v parameter
   -> Vector -- ^ dpdu
   -> Vector -- ^ dpdv
   -> Vector -- ^ dndu
   -> Vector -- ^ dndv
   -> (Float, Float) -- ^ barycentrics for triangle hits
   -> DifferentialGeometry
mkDgTri p u v dpdu dpdv dndu dndv bs = DG p n u v dpdu dpdv dndu dndv (Just bs) where
   n = normalize $ dpdu `cross` dpdv

-- | transforms a @DifferentialGeometry@
transDg :: Transform -> DifferentialGeometry -> DifferentialGeometry
{-# INLINE transDg #-}
transDg t (DG p n u v dpdu dpdv dndu dndv bs) = dg' where
   dg' = DG p' n' u v dpdu' dpdv' dndu' dndv' bs
   p' = transPoint t p
   n' = normalize $ transNormal t n
   dpdu' = transVector t dpdu
   dpdv' = transVector t dpdv
   dndu' = transNormal t dndu
   dndv' = transNormal t dndv
