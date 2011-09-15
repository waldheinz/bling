{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow(..), Sample(..), Sampler(..), Sampled, mkRandomSampler, mkStratifiedSampler, 

   -- * Sampling

   rnd, rnd2D, rnd', rnd2D', coverWindow, splitWindow, shiftToPixel,

   -- * Running Sampled Computations
   
   runSampled, randToSampled,
   
   -- * Accessing Camera Samples

   imageX, imageY, lensUV,

   samples
   
   ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad as CM
import Data.List (transpose, zipWith4)
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

data Sample = Sample {
   smpImageX :: ! Float,
   smpImageY :: ! Float,
   smpLens :: ! R.Rand2D,
   smpRnd1D :: ! (V.Vector Float),
   smpRnd2D :: ! (V.Vector R.Rand2D)
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

data Sampler = Random !Int | Stratified !Int !Int


mkRandomSampler :: Int -> Sampler
mkRandomSampler = Random

mkStratifiedSampler :: Int -> Int -> Sampler
mkStratifiedSampler = Stratified

samples :: (PrimMonad m) => Sampler -> SampleWindow -> Int -> Int -> R.Rand m [Sample]
samples (Random spp) w _ _ = Prelude.mapM smp ws where
      ws = Prelude.concatMap (Prelude.replicate spp) (coverWindow w)
  --    smp :: (Int, Int) -> R.Rand Sample
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

mkSamples
   :: [(Flt, Flt)] -- ^ pixel coordinates
   -> [R.Rand2D] -- ^ lens coordinates
   -> [V.Vector Flt] -- ^ 1d samples
   -> [V.Vector R.Rand2D] -- ^ 2d samples
   -> [Sample]
mkSamples = Data.List.zipWith4 go where
   go (px, py) lens r1d r2d = Sample px py lens r1d r2d

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
   vals <- CM.replicateM n $ (stratified1D nu) >>= shuffle nu
   return $ vectorize vals

mk2D :: PrimMonad m => Int -> Int -> Int -> R.Rand m [V.Vector R.Rand2D]
mk2D nu nv n = do
   vals <- CM.replicateM n $ (stratified2D nu nv) >>= shuffle (nu*nv)
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


{-
class (PrimMonad m) => Sampler m a where
   samples :: a -> SampleWindow -> Int -> Int -> R.Rand m [Sample]


data AnySampler a = Sampler (PrimState m) a => MkAnySampler a

mkAnySampler :: (Sampler a) => a -> AnySampler
mkAnySampler = MkAnySampler

instance (PrimMonad m) => Sampler m AnySampler where
   samples (MkAnySampler s) = samples s
   -}
   
newtype Sampled m a = Sampled {
   runSampled :: ReaderT Sample (R.Rand m) a
   } deriving (Monad, MonadReader Sample)

--runSampled :: Seed -> Sample -> Sampled a -> a
-- {-# INLINE runSampled #-}
--runSampled seed smp k = R.runWithSeed seed (runReaderT (runS k) smp)

-- | upgrades from @Rand@ to @Sampled@
randToSampled
   :: (PrimMonad m)
   => Sampled m a -- ^ the sampled computation
   -> Sample -- ^ the sample to use
   -> R.Rand m a
randToSampled = runReaderT . runSampled

rnd :: (PrimMonad m) => Sampled m Float
rnd = Sampled (lift R.rnd)
{-# INLINE rnd #-}

rnd2D :: (PrimMonad m) => Sampled m R.Rand2D
rnd2D = Sampled (lift R.rnd2D)
{-# INLINE rnd2D #-}

imageX :: (PrimMonad m) => Sampled m Float
imageX = smpImageX `liftM` ask
{-# INLINE imageX #-}

imageY :: (PrimMonad m) => Sampled m Float
imageY = smpImageY `liftM` ask
{-# INLINE imageY #-}

lensUV :: (PrimMonad m) => Sampled m R.Rand2D
lensUV = smpLens `liftM` ask
{-# INLINE lensUV #-}

rnd' :: (PrimMonad m) => Int -> Sampled m Float
{-# INLINE rnd' #-}
rnd' n = do
   r1d <- smpRnd1D `liftM` ask
   if V.length r1d > n
      then return $ V.unsafeIndex r1d n
      else Sampled (lift R.rnd)

rnd2D' :: (PrimMonad m) => Int -> Sampled m R.Rand2D
{-# INLINE rnd2D' #-}
rnd2D' n = do
   r2d <- smpRnd2D `liftM` ask
   if V.length r2d > n
      then return $ V.unsafeIndex r2d n
      else Sampled (lift R.rnd2D)
