{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow, Sampler
   
   ) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef
import Data.Vector.Unboxed as V
import System.Random.MWC

import Graphics.Bling.Math
import Graphics.Bling.Random as R


-- | An (image) region which should be covered with samples
data SampleWindow = SampleWindow {
   xStart :: ! Int, -- ^ first image row to cover
   xEnd :: ! Int, -- ^ last row to cover
   yStart :: ! Int, -- ^ first line to cover
   yEnd :: ! Int, -- ^ last line to cover
   spp :: ! Int -- ^ samples per pixel
   }

class Sampler a where

data Sample = Sample {
   imageX :: Float,
   imageY :: Float,
   lens :: Rand2D,
   rnd2D :: V.Vector Rand2D,
   rnd1D :: V.Vector Float
   }

newtype Sampled a = Sampled {
   runS :: ReaderT Sample Rand a
   } deriving (Monad, MonadReader Sample, MonadTrans Rand)

runSampled :: Seed -> Sample -> Sampled a -> a
runSampled seed sample k = runRand' seed (runReaderT (runS k) sample)

rnd' :: Int -> Sampled Float
rnd' n = do
   smp <- ask
   let r1d = rnd1D smp
   
   if V.length r1d < n
      then return $ V.unsafeIndex r1d n
      else rnd
