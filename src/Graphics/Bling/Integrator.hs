{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.Bling.Integrator (
   
   SurfaceIntegrator(..), Contribution, Consumer, mkContrib,
   
   -- * Existentials
   mkAnySurface, AnySurfaceIntegrator
   
   ) where

import Control.Monad.ST

import Graphics.Bling.Math
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

mkContrib
   :: WeightedSpectrum
   -> Bool -- ^ true -> splat, otherwise -> addSample
   -> Sampled m Contribution
{-# INLINE mkContrib #-}
mkContrib ws splat = do
   cs <- cameraSample
   return $ (splat, (imageX cs, imageY cs, ws))
   
type Consumer m = Contribution -> ST m ()

class Printable a => SurfaceIntegrator a where
   contrib :: a -> Scene -> Consumer s -> Ray -> Sampled s ()
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
   
