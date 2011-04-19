{-# LANGUAGE RankNTypes #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow, Sampler
   
   ) where

import Control.Monad.ST
import Data.Vector.Unboxed as V
import System.Random.MWC

import Graphics.Bling.Math

-- | An (image) region which should be covered with samples
data SampleWindow = SampleWindow {
   xStart :: ! Int, -- ^ first image row to cover
   xEnd :: ! Int, -- ^ last row to cover
   yStart :: ! Int, -- ^ first line to cover
   yEnd :: ! Int, -- ^ last line to cover
   spp :: ! Int -- ^ samples per pixel
   }

type Rand2D = (Float, Float)

class Sampler a where
   
data Sample a = Sample {
   rng :: forall s. Gen s -> ST s a,
   rnd2D :: V.Vector Rand2D,
   rnd1D :: V.Vector Float
   }

