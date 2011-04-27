{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow(..), Sample(..), Sampler(..), Sampled,
   AnySampler, mkAnySampler,

   -- * Sampling

   rnd, rnd2D, rnd', coverWindow,

   -- * Running Sampled Computations
   
   runSampled, runSampledIO, runSampledRand,
   
   -- * Accessing Camera Samples

   imageX, imageY, lensUV
   
   ) where

import Control.Monad.Reader
import Data.Vector.Unboxed as V
import System.Random.MWC

import qualified Graphics.Bling.Random as R

-- | An (image) region which should be covered with samples
data SampleWindow = SampleWindow {
   xStart :: ! Int, -- ^ first image row to cover
   xEnd :: ! Int, -- ^ last row to cover
   yStart :: ! Int, -- ^ first line to cover
   yEnd :: ! Int -- ^ last line to cover
   }

data Sample = Sample {
   smpImageX :: ! Float,
   smpImageY :: ! Float,
   smpLens :: ! R.Rand2D,
   smpRnd2D :: ! (V.Vector R.Rand2D),
   smpRnd1D :: ! (V.Vector Float)
   } deriving (Show)

coverWindow :: SampleWindow -> [(Int, Int)]
coverWindow w = [(x, y) | x <- [xStart w .. xEnd w], y <- [yStart w .. yEnd w]]

class Sampler a where
   samples :: a -> SampleWindow -> R.Rand [Sample]

data AnySampler = forall a . Sampler a => MkAnySampler a

mkAnySampler :: (Sampler a) => a -> AnySampler
mkAnySampler = MkAnySampler

instance Sampler AnySampler where
   samples (MkAnySampler s) = samples s

newtype Sampled a = Sampled {
   runS :: ReaderT Sample R.Rand a
   } deriving (Monad, MonadReader Sample)

runSampled :: Seed -> Sample -> Sampled a -> a
{-# INLINE runSampled #-}
runSampled seed smp k = R.runRand' seed (runReaderT (runS k) smp)

runSampledIO :: Sample -> Sampled a -> IO a
runSampledIO smp k = R.runRandIO (runReaderT (runS k) smp)
{-# INLINE runSampledIO #-}

-- | upgrades from @Rand@ to @Sampled@
runSampledRand
   :: Sample -- ^ the sample to use
   -> Sampled a -- ^ the sampled computation
   -> R.Rand a
runSampledRand smp k = runReaderT (runS k) smp

rnd :: Sampled Float
rnd = Sampled (lift R.rnd)
{-# INLINE rnd #-}

rnd2D :: Sampled R.Rand2D
rnd2D = Sampled (lift R.rnd2D)
{-# INLINE rnd2D #-}

imageX :: Sampled Float
imageX = smpImageX `liftM` ask
{-# INLINE imageX #-}

imageY :: Sampled Float
imageY = smpImageY `liftM` ask
{-# INLINE imageY #-}

lensUV :: Sampled R.Rand2D
lensUV = smpLens `liftM` ask
{-# INLINE lensUV #-}

rnd' :: Int -> Sampled Float
{-# INLINE rnd' #-}
rnd' n = do
   r1d <- smpRnd1D `liftM` ask
   if V.length r1d < n
      then return $ V.unsafeIndex r1d n
      else Sampled (lift R.rnd)


