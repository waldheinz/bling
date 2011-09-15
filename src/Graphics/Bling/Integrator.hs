{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Integrator (
   
   SurfaceIntegrator(..), Contribution, mkContrib,
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Control.Monad.Primitive

import Graphics.Bling.Math
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

type Contribution = [ImageSample]

mkContrib :: PrimMonad m => WeightedSpectrum -> Sampled m Contribution
mkContrib ws = do
   x <- imageX
   y <- imageY
   return $ [ImageSample x y ws]
   
class Printable a => SurfaceIntegrator a where
   contrib :: (PrimMonad m) => a -> Scene -> Ray -> Sampled m Contribution
   sampleCount1D :: a -> Int
   sampleCount2D :: a -> Int
   
data AnySurfaceIntegrator =
   forall a . SurfaceIntegrator a => MkAnySurfaceIntegrator a

mkAnySurface :: (SurfaceIntegrator a) => a -> AnySurfaceIntegrator
mkAnySurface = MkAnySurfaceIntegrator

instance Printable AnySurfaceIntegrator where
   prettyPrint (MkAnySurfaceIntegrator a) = prettyPrint a

instance SurfaceIntegrator AnySurfaceIntegrator where
   contrib (MkAnySurfaceIntegrator a) = contrib a
   sampleCount1D (MkAnySurfaceIntegrator a) = sampleCount1D a
   sampleCount2D (MkAnySurfaceIntegrator a) = sampleCount2D a
   