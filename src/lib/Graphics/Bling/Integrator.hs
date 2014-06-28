
{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Graphics.Bling.Integrator (
   
   SurfaceIntegrator(..)
   ) where


import Control.Monad.Primitive
import Control.Monad.ST


import Graphics.Bling.Math
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

{-
mkContrib
   :: WeightedSpectrum
   -> Bool -- ^ true -> splat, otherwise -> addSample
   -> Sampled m Contribution
{-# INLINE mkContrib #-}
mkContrib ws splat = do
   cs <- cameraSample
   return $ (splat, (imageX cs, imageY cs, ws))
   
type Consumer m = Contribution -> ST m ()
-}

data SurfaceIntegrator = SurfaceIntegrator
   { surfaceLi       :: (PrimMonad (ST s)) => Scene -> Ray -> Sampled s Spectrum
   , sampleCount1D   :: {-# UNPACK #-} !Int
   , sampleCount2D   :: {-# UNPACK #-} !Int
   }

