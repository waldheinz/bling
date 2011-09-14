{-# LANGUAGE RankNTypes #-}

module Graphics.Bling.Random (

   -- * managing the random number generator
   
   Rand, Rand2D, runRand, runRand', runRandST, runRandIO, mkRndGen,
      
   -- * generating random values
   
   rnd2D, rnd, rndList, rndList2D, rndInt, rndIntList
   ) where

import Control.Monad (liftM)
import Control.Monad.ST
import Data.Vector.Unboxed
import System.Random.MWC

type Rand2D = (Float, Float)

-- | Marks a computation that requires random values
newtype Rand a = Rand {
   rng :: forall s. Gen s -> ST s a
   }

{-# INLINE rng #-}

-- | For allowing the Monadic syntax when using @Rand@
instance Monad Rand where
    return k = Rand (\ _ -> return k)
    Rand c1 >>= fc2 = Rand (\ g -> c1 g >>= \a -> rng (fc2 a) g)

-- | Lets a computation run in the Rand Monad
runRand :: Int -> Rand a -> a
runRand seed (Rand c) = runST $ do
   gen <- mkRndGen seed
   c gen
   
runRand' :: Seed -> Rand a -> a
runRand' seed (Rand c) = runST $ do
   gen <- restore seed
   c gen

runRandIO :: Rand a -> IO a
runRandIO (Rand c) = withSystemRandom c

mkRndGen :: Int -> ST s (Gen s)
mkRndGen seed = initialize $ singleton $ fromIntegral seed

runRandST :: forall s t. Rand t -> Gen s -> ST s t
runRandST (Rand c) gen  = c gen

-- | Provides a random @Float@ in @[0..1)@
rnd :: Rand Float
{-# INLINE rnd #-}
rnd = do
   u <- Rand uniform
   return $ u - 2**(-33)

rndInt :: Rand Int
rndInt = Rand uniform

rndIntList
   :: Int
   -> Rand [Int]
rndIntList n
   | n <= 0 = return []
   | otherwise = do
      u <- rndInt
      (liftM . (:)) u $ rndIntList (n-1)

-- | generates a list of given length of random numbers in [0..1)
rndList
   :: Int -- ^ the length of the list to generate
   -> Rand [Float]
rndList n
   | n <= 0 = return []
   | otherwise = do
      u <- rnd
      (liftM . (:)) u $ rndList (n-1)

-- | generates a list of given length containing tuples of random numbers
rndList2D
   :: Int -- the length of the list to generate
   -> Rand [Rand2D]
rndList2D n = do
   us <- rndList n
   vs <- rndList n
   return $ Prelude.zip us vs

rnd2D :: Rand Rand2D
{-# INLINE rnd2D #-}
rnd2D = do
   u1 <- rnd
   u2 <- rnd
   return (u1, u2)
