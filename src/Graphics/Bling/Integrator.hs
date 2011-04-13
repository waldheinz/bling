{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Integrator (
   SurfaceIntegrator(..),
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Text.PrettyPrint

import Graphics.Bling.Math
import Graphics.Bling.Random
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

class SurfaceIntegrator a where
   li :: a -> Scene -> Ray -> Rand WeightedSpectrum
   
   -- | an @Integrator@ should be able to briefly describe itself
   pp :: a -> Doc
   
data AnySurfaceIntegrator =
   forall a . SurfaceIntegrator a => MkAnySurfaceIntegrator a

mkAnySurface :: (SurfaceIntegrator a) => a -> AnySurfaceIntegrator
mkAnySurface = MkAnySurfaceIntegrator

instance SurfaceIntegrator AnySurfaceIntegrator where
   li (MkAnySurfaceIntegrator a) = li a
   pp (MkAnySurfaceIntegrator a) = pp a
   