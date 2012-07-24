{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow(..), Sampler, Sampled, mkRandomSampler, mkStratifiedSampler,
   mkStratifiedSampler',
   
   -- * Sampling
   Sample(..), STVector, mkPrecompSample,
   rnd, rnd2D, rnd', rnd2D', coverWindow, splitWindow, shiftToPixel,
   
   -- * Running Sampled Computations
   
   runSampled, randToSampled, sample, liftSampled,
   
   -- * Accessing Camera Samples

   CameraSample(..), cameraSample
   
   ) where

import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef
import Control.Monad as CM
import qualified Data.Vector.Unboxed.Mutable as V
import System.Random
import qualified System.Random.Shuffle as S

import Graphics.Bling.Math
import qualified Graphics.Bling.Random as R

-- | An (image) region which should be covered with samples
data SampleWindow = SampleWindow {
   xStart   :: {-# UNPACK #-} ! Int,   -- ^ first image row to cover
   xEnd     :: {-# UNPACK #-} ! Int,   -- ^ last row to cover
   yStart   :: {-# UNPACK #-} ! Int,   -- ^ first line to cover
   yEnd     :: {-# UNPACK #-} ! Int    -- ^ last line to cover
   } deriving (Show)

instance NFData SampleWindow where
   -- all fields are strict, so the default implementation should be sufficient

data CameraSample = CameraSample {
   imageX :: {-# UNPACK #-} ! Float,
   imageY :: {-# UNPACK #-} ! Float,
   lensUV :: {-# UNPACK #-} ! R.Rand2D
   } deriving (Show)

coverWindow :: SampleWindow -> [(Int, Int)]
coverWindow w = [(x, y) | y <- [yStart w .. yEnd w], x <- [xStart w .. xEnd w]]

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

type STVector s a = V.MVector (PrimState (ST s)) a

data Sample s
   = RandomSample {-# UNPACK #-} ! CameraSample
   | PrecomSample {-# UNPACK #-} ! CameraSample !(STVector s Flt) !(STVector s R.Rand2D)

mkPrecompSample :: CameraSample -> (STVector s Flt) -> (STVector s R.Rand2D) -> (Sample s)
mkPrecompSample = PrecomSample

data Sampler
   = Random {-#UNPACK #-} !Int
   | Stratified {-# UNPACK #-} !Int {-# UNPACK #-} !Int

mkRandomSampler :: Int -> Sampler
mkRandomSampler = Random

mkStratifiedSampler :: Int -> Int -> Sampler
mkStratifiedSampler nx ny
   | nx <= 0 || ny <= 0 = error "mkStratifiedSampler: nx and ny must be > 0"
   | otherwise = Stratified nx ny

mkStratifiedSampler' :: Int -> Sampler
mkStratifiedSampler' spp = mkStratifiedSampler spp' spp' where
   spp' = max 1 $ ceiling $ sqrt $ (fromIntegral spp :: Float)

sample :: Sampler -> SampleWindow -> Int -> Int -> Sampled s a -> R.Rand s [a]
sample (Random spp) wnd _ _ c = {-# SCC "sample.Random" #-} do
   liftM concat $ forM (coverWindow wnd) $ \ (ix, iy) -> do
      let (fx, fy) = (fromIntegral ix, fromIntegral iy)
      CM.replicateM spp $ do
         ox <- R.rnd
         oy <- R.rnd
         luv <- R.rnd2D
         let s = RandomSample (CameraSample (fx + ox) (fy + oy) luv)
         randToSampled c s

sample (Stratified nu nv) wnd n1d n2d c = {-# SCC "sample.Stratified" #-} do
   v1d <- R.liftR $ V.new (nu * nv * n1d)
   v2d <- R.liftR $ V.new (nu * nv * n2d)
   
   liftM concat $ forM (coverWindow wnd) $ \ (ix, iy) -> do
      ps <- stratified2D nu nv -- pixel samples
      lens <- stratified2D nu nv >>= shuffle (nu*nv) -- lens samples
      let (fx, fy) = (fromIntegral ix, fromIntegral iy)
      
      fill v1d n1d (nu*nv) (stratified1D (nu*nv))
      fill v2d n2d (nu*nv) (stratified2D nu nv)
      
      forM (zip3 ps lens [0..]) $ \((ox, oy), luv, n) ->
         let
            cs = CameraSample (fx + ox) (fy + oy) luv
            s = PrecomSample cs (V.slice (n*n1d) n1d v1d) (V.slice (n*n2d) n2d v2d)
         in randToSampled c s
         
-- | shuffles a list
shuffle
   :: Int -- ^ the length of the list
   -> [a] -- ^ the list to shuffle
   -> R.Rand m [a]
shuffle xl xs = do
   seed <- R.rndInt
   return $ {-# SCC "shuffle'" #-} S.shuffle' xs xl $ mkStdGen seed

fill :: (V.Unbox a) => V.MVector (PrimState (ST m)) a -> Int -> Int -> (R.Rand m [a]) -> R.Rand m ()
fill v n n' gen = {-# SCC "fill" #-} do
   forM_ [0..n-1] $ \off -> do
      rs <- {-# SCC "fill.top" #-} do
         vv <- R.liftR $ V.new n'
         xs <- gen
         forM_ (zip xs [0..]) $ \(val, i) -> do
            R.liftR $ V.write vv i val
         return vv

      {-# SCC "fill.shuffle" #-} R.shuffle rs
      
      idx <- R.newRandRef 0
      {-# SCC "fill.bottom" #-} R.liftR $ forM_ [0..n'-1] $ \ vidx -> do
         i <- readSTRef idx
         val <- V.read rs vidx
         modifySTRef idx (+1)
         V.write v (i * n + off) val

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
{-# INLINE rnd' #-}
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
