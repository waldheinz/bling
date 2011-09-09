

module Graphics.Bling.DifferentialGeometry (
   module Graphics.Bling.Transform,
   
   -- * Differential Geometry

   DifferentialGeometry, mkDg, mkDg',
      dgP, dgN, dgU, dgV, dgDPDU, dgDPDV, dgDNDU, dgDNDV,
      transDg

   ) where

import Graphics.Bling.Transform

--
-- Differential Geometry
--

data DifferentialGeometry = DG {
   dgP :: {-# UNPACK #-} ! Point,
   dgN :: {-# UNPACK #-} ! Normal,
   dgU :: {-# UNPACK #-} ! Flt,
   dgV :: {-# UNPACK #-} ! Flt,
   dgDPDU :: {-# UNPACK #-} ! Vector,
   dgDPDV :: {-# UNPACK #-} ! Vector,
   dgDNDU :: {-# UNPACK #-} ! Vector,
   dgDNDV :: {-# UNPACK #-} ! Vector
   } deriving (Show)

mkDg
   :: Point -- ^ intersection point
   -> Flt -- ^ u parameter
   -> Flt -- ^ v parameter
   -> Vector -- ^ dpdu
   -> Vector -- ^ dpdv
   -> Vector -- ^ dndu
   -> Vector -- ^ dndv
   -> DifferentialGeometry
mkDg p u v dpdu dpdv dndu dndv = DG p n u v dpdu dpdv dndu dndv where
   n = normalize $ dpdu `cross` dpdv

mkDg' :: Point -> Normal -> DifferentialGeometry
mkDg' p n = DG p n 0 0 dpdu dpdv dn dn where
   (LocalCoordinates dpdu dpdv _) = coordinateSystem n
   dn = mkV (0, 0, 0)
   
-- | transforms a @DifferentialGeometry@
transDg :: Transform -> DifferentialGeometry -> DifferentialGeometry
{-# INLINE transDg #-}
transDg t (DG p n u v dpdu dpdv dndu dndv) = dg' where
   dg' = DG p' n' u v dpdu' dpdv' dndu' dndv'
   p' = transPoint t p
   n' = transNormal t n
   dpdu' = transVector t dpdu
   dpdv' = transVector t dpdv
   dndu' = transNormal t dndu
   dndv' = transNormal t dndv
   