{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Bling.Random (

   -- * managing the random number generator
   
   Rand, Rand2D, runRand, runRandIO, runWithSeed, {- runRandST, runRandIO, mkRndGen, -}
      
   -- * generating random values
   
   rnd2D, rnd, rndList, rndList2D, rndInt, rndIntList
   ) where

import Control.Monad (forM_, replicateM)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Vector.Unboxed hiding (forM_, replicateM, create)
import qualified Data.Vector.Unboxed.Mutable as MV
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

toRand :: PrimMonad m => (MWC.Gen (PrimState m) -> m a) -> Rand m a
toRand = Rand
   
-- | Lets a computation run in the Rand Monad
--runRand :: Int -> Rand a -> a
--runRand seed (Rand c) = runST $ do
--   gen <- mkRndGen seed
--   c gen
   
--runRand' :: Seed -> Rand a -> a
--runRand' seed (Rand c) = runST $ do
--   gen <- restore seed
--   c gen

-- | Run monad using seed
runWithSeed :: PrimMonad m => MWC.Seed -> Rand m a -> m a
runWithSeed seed m = runRand m =<< MWC.restore seed
{-# INLINE runWithSeed #-}

runRandIO :: Rand IO a -> IO a
runRandIO = MWC.withSystemRandom . runRand
{-# INLINE runRandIO #-}

--mkRndGen :: Int -> ST s (Gen s)
--mkRndGen seed = initialize $ singleton $ fromIntegral seed

--runRandST :: forall s t. Rand t -> Gen s -> ST s t
--runRandST (Rand c) gen  = c gen

-- shuffle' :: (PrimMonad m, MV.MVector v a) => v (PrimState m) a -> Rand ()
shuffle' v = do
   forM_ [0..n-1] $ \i -> do
      other <- rndInt --  uniformR (i, n - i - 1) gen
      MV.swap v i other
   where
      n = MV.length v

-- | Provides a random @Float@ in @[0..1)@
rnd :: PrimMonad m => Rand m Float
{-# INLINE rnd #-}
rnd = do
   u <- Rand MWC.uniform
   return $ u - 2**(-33)

rndInt :: PrimMonad m => Rand m Int
rndInt = Rand MWC.uniform

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

-- | generates a list of given length containing tuples of random numbers
rndList2D
   :: PrimMonad m
   => Int -- the length of the list to generate
   -> Rand m [Rand2D]
rndList2D n = do
   us <- rndList n
   vs <- rndList n
   return $ Prelude.zip us vs

rnd2D :: PrimMonad m => Rand m Rand2D
{-# INLINE rnd2D #-}
rnd2D = do
   u1 <- rnd
   u2 <- rnd
   return (u1, u2)
