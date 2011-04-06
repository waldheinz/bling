{-# LANGUAGE RankNTypes #-}

module Graphics.Bling.Random (
   Rand, Rand2D, runRand, runRandST, mkRndGen, rnd2D, rnd
   ) where

import Control.Monad.ST
import Data.Vector.Unboxed
import System.Random.MWC

type Rand2D = (Float, Float)

-- | Marks a computation that requires random values
newtype Rand a = Rand {
   unR :: forall s. Gen s -> ST s a
   }

-- | For allowing the Monadic syntax when using @Rand@
instance Monad Rand where
    return k = Rand (\ _ -> return k)
    Rand c1 >>= fc2 = Rand (\ g -> c1 g >>= \a -> unR (fc2 a) g)

-- | Lets a computation run in the Rand Monad
runRand :: Int -> Rand a -> a
runRand seed (Rand c) = runST (do gen <- initialize $ singleton $ fromIntegral seed
                                  c gen)
mkRndGen :: Int -> ST s (Gen s)
mkRndGen seed = initialize $ singleton $ fromIntegral seed

runRandST :: Gen s -> Rand a -> ST s a
runRandST gen (Rand c) = c gen

-- | Provides a random @Float@ in @[0..1)@
rnd :: Rand Float
{-# INLINE rnd #-}
rnd = do
   u <- Rand uniform
   return $ u - 2**(-33)

rnd2D :: Rand Rand2D
{-# INLINE rnd2D #-}
rnd2D = do
   u1 <- rnd
   u2 <- rnd
   return (u1, u2)
   