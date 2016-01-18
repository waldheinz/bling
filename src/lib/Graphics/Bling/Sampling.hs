
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Bling.Sampling (

   -- * Sampling Types
   SampleWindow(..), Sampler, Sampled, mkRandomSampler, mkStratifiedSampler,
   windowPixels, mkStratifiedSampler',

   -- * Sampling
   Sample(..), STVector, mkPrecompSample,
   rnd, rnd2D, rnd', rnd2D', coverWindow, splitWindow, shiftToPixel,

   -- * Running Sampled Computations

   runSampled, randToSampled, runSample, liftSampled,

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
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Graphics.Bling.Random as R

-- | An (image) region which should be covered with samples
data SampleWindow = SampleWindow {
   xStart   :: {-# UNPACK #-} ! Int,   -- ^ first image row to cover
   xEnd     :: {-# UNPACK #-} ! Int,   -- ^ last row to cover
   yStart   :: {-# UNPACK #-} ! Int,   -- ^ first line to cover
   yEnd     :: {-# UNPACK #-} ! Int    -- ^ last line to cover
   } deriving (Show)

instance NFData SampleWindow where
    rnf x = seq x ()

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

windowPixels :: SampleWindow -> Int
windowPixels wnd = w * h where
   (w, h) = (xEnd wnd - xStart wnd + 1, yEnd wnd - yStart wnd + 1)

shiftToPixel
   :: Int -- ^ pixel x ordinate
   -> Int -- ^ pixel y ordinate
   -> [R.Rand2D]
   -> [(Float, Float)]
shiftToPixel px py = Prelude.map (s (fromIntegral px) (fromIntegral py)) where
   s fx fy (u, v) = (u + fx, v + fy)

--------------------------------------------------------------------------------
-- Samplers
--------------------------------------------------------------------------------

type STVector s a = V.MVector (PrimState (ST s)) a

data Sample s
   = RandomSample {-# UNPACK #-} ! CameraSample
   | PrecomSample {-# UNPACK #-} ! CameraSample !(STVector s Float) !(STVector s R.Rand2D)

mkPrecompSample :: CameraSample -> (STVector s Float) -> (STVector s R.Rand2D) -> (Sample s)
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

runSample :: Sampler -> SampleWindow -> Int -> Int -> Sampled s () -> R.Rand s ()
runSample (Random spp) wnd _ _ c = {-# SCC "sample.Random" #-} do
   forM_ (coverWindow wnd) $ \ (ix, iy) -> do
      let (fx, fy) = (fromIntegral ix, fromIntegral iy)
      CM.replicateM_ spp $ do
         ox <- R.rnd
         oy <- R.rnd
         luv <- R.rnd2D
         let s = RandomSample (CameraSample (fx + ox) (fy + oy) luv)
         randToSampled c s

runSample (Stratified nu nv) wnd n1d n2d c = {-# SCC "sample.Stratified" #-} do
   v1d <- R.liftR $ V.new (nu * nv * n1d)
   v2d <- R.liftR $ V.new (nu * nv * n2d)

   forM_ (coverWindow wnd) $ \ (ix, iy) -> do
      ps <- stratified2D nu nv -- pixel samples
      lens <- stratified2D nu nv >>= \x -> do -- lens samples
         x' <- R.liftR $ UV.thaw x
         R.shuffle x'
         R.liftR $ UV.freeze x'

      let (fx, fy) = (fromIntegral ix, fromIntegral iy)

      fill v1d n1d (nu*nv) (stratified1D (nu*nv))
      fill v2d n2d (nu*nv) (stratified2D nu nv)

      UV.forM_ (UV.izipWith (\i p l -> (p, l, i)) ps lens) $ \((ox, oy), luv, n) ->
         let
            cs = CameraSample (fx + ox) (fy + oy) luv
            s = PrecomSample cs (V.slice (n*n1d) n1d v1d) (V.slice (n*n2d) n2d v2d)
         in randToSampled c s

fill :: (V.Unbox a) => V.MVector (PrimState (ST m)) a -> Int -> Int -> (R.Rand m (UV.Vector a)) -> R.Rand m ()
fill v n n' gen = {-# SCC "fill" #-} do
   forM_ [0..n-1] $ \off -> do
{-      rs <- {-# SCC "fill.top" #-} do
         vv <- R.liftR $ V.new n'
         xs <- gen
         forM_ (zip xs [0..]) $ \(val, i) -> do
            R.liftR $ V.write vv i val
         return vv-}
      rs <- gen >>= R.liftR . UV.thaw

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
   -> R.Rand m (UV.Vector Float)
stratified1D n = let du = 1 / fromIntegral n in
   R.rndVec n >>= \x -> return $! GV.imap (\i v -> min almostOne ((fromIntegral i + v) * du)) x

-- | generates stratified samples in two dimensions
stratified2D
   :: Int -- ^ number of samples in first dimension
   -> Int -- ^ number of samples in second dimension
   -> R.Rand m (UV.Vector (Float, Float))
stratified2D nu nv = let (du, dv) = (1 / fromIntegral nu, 1 / fromIntegral nv) in do
   js <- R.rndVec2D (nu * nv)
   return $! GV.imap (\i (ju, jv) -> let (u, v) = quotRem i nu in
      (min almostOne ((fromIntegral u + ju)*du), min almostOne ((fromIntegral v + jv)*dv))) js

newtype Sampled m a = Sampled {
   runSampled :: ReaderT (Sample m) (R.Rand m) a
   } deriving (Applicative, Functor, Monad, MonadReader (Sample m))

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
