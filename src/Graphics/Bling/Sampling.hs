{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.ST
import Control.Monad as CM
import qualified Data.Vector.Unboxed.Mutable as V
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

data Sample s
   = RandomSample {-# UNPACK #-} ! CameraSample
   | PrecomSample {-# UNPACK #-} ! CameraSample !(V.MVector (PrimState (ST s)) Flt) !(V.MVector (PrimState (ST s)) R.Rand2D)

data Sampler = Random !Int | Stratified !Int !Int

mkRandomSampler :: Int -> Sampler
mkRandomSampler = Random

mkStratifiedSampler :: Int -> Int -> Sampler
mkStratifiedSampler = Stratified

sample :: Sampler -> SampleWindow -> Int -> Int -> Sampled s a -> R.Rand s ()
sample (Random spp) wnd _ _ c = do
   {-# SCC "sample.forM_" #-} CM.forM_ (coverWindow wnd) $ \ (ix, iy) -> do
      let (fx, fy) = (fromIntegral ix, fromIntegral iy)
      CM.replicateM spp $ do
         ox <- R.rnd
         oy <- R.rnd
         luv <- R.rnd2D
         let s = RandomSample (CameraSample (fx + ox) (fy + oy) luv)
         randToSampled c s

sample (Stratified nu nv) wnd n1d n2d c = do
   v1d <- R.liftR $ V.replicate (nu * nv * n1d) (0 :: Flt)
   v2d <- R.liftR $ V.replicate (nu * nv * n2d) ((0,0) :: R.Rand2D)
   
   CM.forM_ (coverWindow wnd) $ \ (ix, iy) -> do
      ps <- stratified2D nu nv -- pixel samples
      lens <- stratified2D nu nv >>= shuffle (nu*nv) -- lens samples
      let (fx, fy) = (fromIntegral ix, fromIntegral iy)
      
      fill n1d v1d (stratified1D (nu*nv) >>= shuffle (nu*nv))
      fill n2d v2d (stratified2D nu nv >>= shuffle (nu * nv))
      
      CM.forM_ (zip3 ps lens [0..]) $ \((ox, oy), luv, n) -> do
         let cs = CameraSample (fx + ox) (fy + oy) luv
         let s = PrecomSample cs (V.slice (n*n1d) n1d v1d) (V.slice (n*n2d) n2d v2d)
         randToSampled c s
         
-- | shuffles a list
shuffle
   :: Int -- ^ the length of the list
   -> [a] -- ^ the list to shuffle
   -> R.Rand m [a]
shuffle xl xs = do
   seed <- R.rndInt
   return $ {-# SCC "shuffle'" #-} S.shuffle' xs xl $ mkStdGen seed

fill :: (V.Unbox a) => Int -> V.MVector (PrimState (ST m)) a -> (R.Rand m [a]) -> R.Rand m ()
fill n v gen = do
   forM_ [0..n-1] $ \off -> do
      rs <- gen
      forM_ (zip rs [0..]) $ \ (val, idx) -> do
         R.liftR $ V.write v (idx * n + off) val

almostOne :: Float
almostOne = 0.9999999403953552 -- 0x1.fffffep-1

stratified1D
   :: Int
   -> R.Rand m [Flt]
stratified1D n = do
   js <- R.rndList n
   return $ Prelude.zipWith j us js where
      du = 1 / fromIntegral n
      j u ju = min almostOne ((u+ju)*du)
      us = [fromIntegral u | u <- [0..(n-1)]]
         
-- | generates stratified samples in two dimensions
stratified2D
   :: Int -- ^ number of samples in first dimension
   -> Int -- ^ number of samples in second dimension
   -> R.Rand m [R.Rand2D]

stratified2D nu nv = do
   js <- R.rndList2D (nu * nv)
   return $ Prelude.zipWith j uvs js where
      (du, dv) = (1 / fromIntegral nu, 1 / fromIntegral nv)
      j (u, v) (ju, jv) = (min almostOne ((u+ju)*du), min almostOne ((v+jv)*dv))
      uvs = [(fromIntegral u, fromIntegral v) | u <- [0..(nu-1)], v <- [0..(nv-1)]]
         
newtype Sampled m a = Sampled {
   runSampled :: ReaderT (Sample m) (R.Rand m) a
   } deriving (Monad, MonadReader (Sample m))

-- | upgrades from @Rand@ to @Sampled@
randToSampled
   :: Sampled m a -- ^ the sampled computation
   -> Sample m -- ^ the sample
   -> R.Rand m a
randToSampled = runReaderT . runSampled
{-# INLINE randToSampled #-}

liftSampled :: ST s a -> Sampled s a
{-# INLINE liftSampled #-}
liftSampled m = Sampled $ lift $ R.liftR m

rnd :: Sampled m Float
rnd = Sampled (lift R.rnd)
{-# INLINE rnd #-}

rnd2D :: Sampled m R.Rand2D
rnd2D = Sampled (lift R.rnd2D)
{-# INLINE rnd2D #-}

cameraSample :: Sampled m CameraSample
cameraSample = ask >>= \s -> case s of
                                  (RandomSample cs) -> return cs
                                  (PrecomSample cs _ _) -> return cs
{-# INLINE cameraSample #-}

rnd' :: Int -> Sampled m Float
--{-# INLINE rnd' #-}
rnd' n = do
   s <- ask
   case s of
        (RandomSample _) -> rnd
        (PrecomSample _ v _) -> if (V.length v > n)
                                   then liftSampled $ V.unsafeRead v n
                                   else rnd

rnd2D' :: Int -> Sampled s R.Rand2D
{-# INLINE rnd2D' #-}
rnd2D' n = do
   s <- ask
   case s of
        (RandomSample _) -> rnd2D
        (PrecomSample _ _ v) -> if (V.length v > n)
                                   then liftSampled $ V.unsafeRead v n
                                   else rnd2D
