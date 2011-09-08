

module Graphics.Bling.DifferentialGeometry (
   module Graphics.Bling.Transform,
   
   -- * Differential Geometry

   DifferentialGeometry, mkDg', dgP, dgN, transDg

   ) where

import Graphics.Bling.Transform

--
-- Differential Geometry
--

data DifferentialGeometry = DifferentialGeometry {
   dgP :: {-# UNPACK #-} ! Point,
   dgN :: {-# UNPACK #-} ! Normal
   } deriving (Show)

mkDg' :: Point -> Normal -> DifferentialGeometry
mkDg' = DifferentialGeometry

-- | transforms a @DifferentialGeometry@ to world space
transDg :: Transform -> DifferentialGeometry -> DifferentialGeometry
{-# INLINE transDg #-}
transDg t (DifferentialGeometry p n) =
   DifferentialGeometry (transPoint t p) (transNormal t n)

