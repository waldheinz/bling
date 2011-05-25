{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Integrator (
   
   SurfaceIntegrator(..), Contribution, mkContrib,
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

type Contribution = [ImageSample]

mkContrib :: WeightedSpectrum -> Sampled Contribution
mkContrib ws = do
   x <- imageX
   y <- imageY
   return $ [ImageSample x y ws]
   
class Printable a => SurfaceIntegrator a where
   contrib :: a -> Scene -> Ray -> Sampled Contribution
   
data AnySurfaceIntegrator =
   forall a . SurfaceIntegrator a => MkAnySurfaceIntegrator a

mkAnySurface :: (SurfaceIntegrator a) => a -> AnySurfaceIntegrator
mkAnySurface = MkAnySurfaceIntegrator

instance Printable AnySurfaceIntegrator where
   prettyPrint (MkAnySurfaceIntegrator a) = prettyPrint a

instance SurfaceIntegrator AnySurfaceIntegrator where
   contrib (MkAnySurfaceIntegrator a) = contrib a
   