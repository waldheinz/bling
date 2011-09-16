{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow(..), Sampler, Sampled, mkRandomSampler, mkStratifiedSampler,

   -- * Sampling

   rnd, rnd2D, rnd', rnd2D', coverWindow, splitWindow, shiftToPixel,

   -- * Running Sampled Computations
   
   runSampled, randToSampled, sample, liftSampled,
   
   -- * Accessing Camera Samples

   CameraSample(..), cameraSample
   
   ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad as CM
import Control.Monad.ST
import Data.List (transpose, zipWith4)
import Data.STRef
import Data.Vector.Unboxed as V
import System.Random
import qualified System.Random.Shuffle as S

import Graphics.Bling.Math
import qualified Graphics.Bling.Random as R

-- | An (image) region which should be covered with samples
data SampleWindow = SampleWindow {
   xStart :: ! Int, -- ^ first image row to cover
   xEnd :: ! Int, -- ^ last row to cover
   yStart :: ! Int, -- ^ first line to cover
   yEnd :: ! Int -- ^ last line to cover
   } deriving (Show)

data CameraSample = CameraSample {
   imageX :: ! Float,
   imageY :: ! Float,
   lensUV :: ! R.Rand2D
   } deriving (Show)

coverWindow :: SampleWindow -> [(Int, Int)]
coverWindow w = [(x, y) | x <- [xStart w .. xEnd w], y <- [yStart w .. yEnd w]]

splitWindow :: SampleWindow -> [SampleWindow]
splitWindow (SampleWindow x0 x1 y0 y1) = ws where
   ws = Prelude.map mkWnd [(x, y) | y <- [y0, y0+16 .. y1], x <- [x0, x0+16 .. x1]]
   mkWnd (x, y) = SampleWindow x (min (x+15) x1) y (min (y+15) y1)

shiftToPixel
   :: Int -- ^ pixel x ordinate
   -> Int -- ^ pixel y ordinate
   -> [R.Rand2D]
   -> [(Flt, Flt)]
shiftToPixel px py = Prelude.map (s (fromIntegral px) (fromIntegral py)) where
   s fx fy (u, v) = (u + fx, v + fy)

--------------------------------------------------------------------------------
-- Samplers
--------------------------------------------------------------------------------

data (PrimMonad m) => SamplingState m = SamplingState
   { getCs :: {-# UNPACK #-} ! CameraSample
   , func1D :: Int -> R.Rand m Flt
   , func2D :: Int -> R.Rand m R.Rand2D
  -- , dsd :: STRef (PrimState m) CameraSample
   }

data Sampler = Random !Int | Stratified !Int !Int

mkRandomSampler :: Int -> Sampler
mkRandomSampler = Random

mkStratifiedSampler :: Int -> Int -> Sampler
mkStratifiedSampler = Stratified

sample :: (PrimMonad m) => Sampler -> SampleWindow -> Int -> Int -> Sampled m a -> R.Rand m ()
sample (Random spp) wnd _ _ c = do
   CM.forM_ (coverWindow wnd) $ \ (ix, iy) -> CM.replicateM spp $ do
      ox <- R.rnd
      oy <- R.rnd
      luv <- R.rnd2D
      randToSampled c $ SamplingState (CameraSample
            ((fromIntegral ix) + ox)
            ((fromIntegral iy) + oy)
            luv) f1d f2d
      
   where
      f1d _ = R.rnd
      f2d _ = R.rnd2D
      
sample _ _ _ _ _ = error "sample not implemented for sampler"

{-
samples :: (PrimMonad m) => Sampler -> SampleWindow -> Int -> Int -> R.Rand m [Sample]
samples (Random spp) w _ _ = Prelude.mapM smp ws where
      ws = Prelude.concatMap (Prelude.replicate spp) (coverWindow w)
      smp (ix, iy) = do
         ox <- R.rnd
         oy <- R.rnd
         luv <- R.rnd2D
         return $ Sample
            ((fromIntegral ix) + ox)
            ((fromIntegral iy) + oy)
            luv
            empty empty
      

samples (Stratified nu nv) w n1d n2d = Prelude.concat `liftM` CM.mapM (pixel nu nv n1d n2d) (coverWindow w)

-- | creates stratified samples for one pixel
pixel :: PrimMonad m => Int -> Int -> Int -> Int -> (Int, Int) -> R.Rand m [Sample]
pixel nu nv n1d n2d (px, py) = do
   lens <- stratified2D nu nv >>= shuffle (nu*nv)
   ps <- stratified2D nu nv
   r1d <- mk1D (nu*nv) n1d
   r2d <- mk2D nu nv n2d

   return $ mkSamples (shiftToPixel px py ps) lens r1d r2d

-- | shuffles a list
shuffle
   :: PrimMonad m
   => Int -- ^ the length of the list
   -> [a] -- ^ the list to shuffle
   -> R.Rand m [a]
shuffle xl xs = do
   seed <- R.rndInt
   return $ {-# SCC "shuffle'" #-} S.shuffle' xs xl $ mkStdGen seed

vectorize :: (V.Unbox a) => [[a]] -> [V.Vector a]
vectorize xs = Prelude.map V.fromList $ transpose xs

mk1D :: PrimMonad m => Int -> Int -> R.Rand m [V.Vector Flt]
mk1D nu n = do
   vals <- CM.replicateM n $ (stratified1D nu)
   return $ vectorize vals

mk2D :: PrimMonad m => Int -> Int -> Int -> R.Rand m [V.Vector R.Rand2D]
mk2D nu nv n = do
   vals <- CM.replicateM n $ (stratified2D nu nv)
   return $ vectorize vals

almostOne :: Float
almostOne = 0.9999999403953552 -- 0x1.fffffep-1

stratified1D
   :: (PrimMonad m)
   => Int
   -> R.Rand m [Flt]
stratified1D n = do
   js <- R.rndList n
   return $ Prelude.zipWith j us js where
      du = 1 / fromIntegral n
      j u ju = min almostOne ((u+ju)*du)
      us = [fromIntegral u | u <- [0..(n-1)]]

-- | generates stratified samples in two dimensions
stratified2D
   :: (PrimMonad m)
   => Int -- ^ number of samples in first dimension
   -> Int -- ^ number of samples in second dimension
   -> R.Rand m [R.Rand2D]

stratified2D nu nv = do
   js <- R.rndList2D (nu * nv)
   return $ Prelude.zipWith j uvs js where
      (du, dv) = (1 / fromIntegral nu, 1 / fromIntegral nv)
      j (u, v) (ju, jv) = (min almostOne ((u+ju)*du), min almostOne ((v+jv)*dv))
      uvs = [(fromIntegral u, fromIntegral v) | u <- [0..(nu-1)], v <- [0..(nv-1)]]
         -}
newtype Sampled m a = Sampled {
   runSampled :: ReaderT (SamplingState m) (R.Rand m) a
   } deriving (Monad, MonadReader (SamplingState m))

-- | upgrades from @Rand@ to @Sampled@
randToSampled
   :: (PrimMonad m)
   => Sampled m a -- ^ the sampled computation
   -> SamplingState m -- ^ the sampler state
   -> R.Rand m a
randToSampled = runReaderT . runSampled

liftSampled :: (PrimMonad m) => m a -> Sampled m a
liftSampled m = Sampled $ lift $ R.liftRand m

rnd :: (PrimMonad m) => Sampled m Float
rnd = Sampled (lift R.rnd)
{-# INLINE rnd #-}

rnd2D :: (PrimMonad m) => Sampled m R.Rand2D
rnd2D = Sampled (lift R.rnd2D)
{-# INLINE rnd2D #-}

cameraSample :: (PrimMonad m) => Sampled m CameraSample
cameraSample = getCs `liftM` ask
{-# INLINE cameraSample #-}

rnd' :: (PrimMonad m) => Int -> Sampled m Float
{-# INLINE rnd' #-}
rnd' n = ask >>= \s -> Sampled $ (lift $ func1D s n)

rnd2D' :: (PrimMonad m) => Int -> Sampled m R.Rand2D
{-# INLINE rnd2D' #-}
rnd2D' n = ask >>= \s -> Sampled $ (lift $ func2D s n)
