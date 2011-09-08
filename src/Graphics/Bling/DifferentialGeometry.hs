

module Graphics.Bling.DifferentialGeometry (
   module Graphics.Bling.Transform,
   
   -- * Differential Geometry

   DifferentialGeometry, mkDg, mkDg', dgP, dgN, dgU, dgV, dgDPDU, dgDPDV,
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
   dgDPDV :: {-# UNPACK #-} ! Vector
   } deriving (Show)

mkDg
   :: Point
   -> Flt
   -> Flt
   -> Vector
   -> Vector
   -> DifferentialGeometry
mkDg p u v dpdu dpdv = DG p n u v dpdu dpdv where
   n = normalize $ dpdu `cross` dpdv

mkDg' :: Point -> Normal -> DifferentialGeometry
mkDg' p n = DG p n 0 0 dpdu dpdv where
   (LocalCoordinates dpdu dpdv _) = coordinateSystem n
   
-- | transforms a @DifferentialGeometry@
transDg :: Transform -> DifferentialGeometry -> DifferentialGeometry
{-# INLINE transDg #-}
transDg t (DG p n u v dpdu dpdv) = DG p' n' u v dpdu' dpdv' where
   p' = transPoint t p
   n' = transNormal t n
   dpdu' = transVector t dpdu
   dpdv' = transVector t dpdv
   