{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Integrator (
   SurfaceIntegrator(..),
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Random
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

class SurfaceIntegrator a where
   li :: a -> Scene -> Ray -> Rand WeightedSpectrum

data AnySurfaceIntegrator =
   forall a . SurfaceIntegrator a => MkAnySurfaceIntegrator a

mkAnySurface :: (SurfaceIntegrator a) => a -> AnySurfaceIntegrator
mkAnySurface = MkAnySurfaceIntegrator

instance SurfaceIntegrator AnySurfaceIntegrator where
   li (MkAnySurfaceIntegrator a) = li a