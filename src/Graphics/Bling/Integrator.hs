{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Integrator (
   SurfaceIntegrator(..), mkImageSample,
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Text.PrettyPrint

import Graphics.Bling.Math
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

class SurfaceIntegrator a where
   li :: a -> Scene -> Ray -> Sampled WeightedSpectrum
   
   -- | an @Integrator@ should be able to briefly describe itself
   pp :: a -> Doc
   
data AnySurfaceIntegrator =
   forall a . SurfaceIntegrator a => MkAnySurfaceIntegrator a

mkAnySurface :: (SurfaceIntegrator a) => a -> AnySurfaceIntegrator
mkAnySurface = MkAnySurfaceIntegrator

instance SurfaceIntegrator AnySurfaceIntegrator where
   li (MkAnySurfaceIntegrator a) = li a
   pp (MkAnySurfaceIntegrator a) = pp a

mkImageSample :: WeightedSpectrum -> Sampled ImageSample
mkImageSample ws = do
   x <- imageX
   y <- imageY
   return $ ImageSample x y ws
   