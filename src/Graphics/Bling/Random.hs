{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Bling.Random (

   -- * managing the random number generator
   
   Rand, Rand2D, runRand, runRandIO, runWithSeed,
      
   -- * generating random values
   
   rnd2D, rnd, rndList, rndList2D, rndInt, rndIntR, rndIntList, shuffle
   ) where

import Control.Monad (forM_, replicateM)
import Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as MV
import qualified System.Random.MWC as MWC

type Rand2D = (Float, Float)

-- | Marks a computation that requires random values
newtype Rand m a = Rand {
   runRand :: MWC.Gen (PrimState m) -> m a
   }
   
-- | For allowing the Monadic syntax when using @Rand@
instance PrimMonad m => Monad (Rand m) where
   return k = Rand (\ _ -> return k)
   Rand c1 >>= fc2 = Rand (\ g -> c1 g >>= \a -> runRand (fc2 a) g)
   {-# INLINE return #-}
   {-# INLINE (>>=)  #-}

-- | Run monad using seed
runWithSeed :: PrimMonad m => MWC.Seed -> Rand m a -> m a
runWithSeed seed m = runRand m =<< MWC.restore seed
{-# INLINE runWithSeed #-}

runRandIO :: Rand IO a -> IO a
runRandIO = MWC.withSystemRandom . runRand
{-# INLINE runRandIO #-}

shuffle :: (PrimMonad m, PrimMonad (Rand m), MV.MVector v a) => v (PrimState (Rand m)) a -> Rand m ()
shuffle v = do
   forM_ [0..n-1] $ \i -> do
      other <- rndIntR (i, n - i - 1)
      MV.swap v i other
   where
      n = MV.length v
{-# INLINE shuffle #-}

-- | Provides a random @Float@ in @[0..1)@
rnd :: PrimMonad m => Rand m Float
{-# INLINE rnd #-}
rnd = do
   u <- Rand MWC.uniform
   return $ u - 2**(-33)

rndInt :: PrimMonad m => Rand m Int
{-# INLINE rndInt #-}
rndInt = Rand MWC.uniform

rndIntR :: PrimMonad m => (Int, Int) -> Rand m Int
{-# INLINE rndIntR #-}
rndIntR r = Rand $ MWC.uniformR r


rndIntList
   :: PrimMonad m
   => Int
   -> Rand m [Int]
rndIntList n = replicateM n $ rndInt

-- | generates a list of given length of random numbers in [0..1)
rndList
   :: PrimMonad m 
   => Int -- ^ the length of the list to generate
   -> Rand m [Float]
rndList n = replicateM n $ rnd
{-# INLINE rndList #-}

-- | generates a list of given length containing tuples of random numbers
rndList2D
   :: PrimMonad m
   => Int -- the length of the list to generate
   -> Rand m [Rand2D]
rndList2D n = do
   us <- rndList n
   vs <- rndList n
   return $ Prelude.zip us vs
{-# INLINE rndList2D #-}

rnd2D :: PrimMonad m => Rand m Rand2D
{-# INLINE rnd2D #-}
rnd2D = do
   u1 <- rnd
   u2 <- rnd
   return (u1, u2)
