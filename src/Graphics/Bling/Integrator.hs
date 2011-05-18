{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Integrator (
   SurfaceIntegrator(..), mkImageSample,
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

class Printable a => SurfaceIntegrator a where
   li :: a -> Scene -> Ray -> Sampled WeightedSpectrum
   
data AnySurfaceIntegrator =
   forall a . SurfaceIntegrator a => MkAnySurfaceIntegrator a

mkAnySurface :: (SurfaceIntegrator a) => a -> AnySurfaceIntegrator
mkAnySurface = MkAnySurfaceIntegrator

instance Printable AnySurfaceIntegrator where
   prettyPrint (MkAnySurfaceIntegrator a) = prettyPrint a

instance SurfaceIntegrator AnySurfaceIntegrator where
   li (MkAnySurfaceIntegrator a) = li a
   
mkImageSample :: WeightedSpectrum -> Sampled ImageSample
mkImageSample ws = do
   x <- imageX
   y <- imageY
   return $ ImageSample x y ws
   