{-# LANGUAGE RankNTypes #-}

module Random (Rand, Rand2D, runRand, rndR, rndRI, rnd) where

import Control.Monad.ST
import Data.Vector.Unboxed
import System.Random.MWC

type Rand2D = (Float, Float)

-- | Marks a computation that requires random values                                                                                         
newtype Rand a = Rand {unR :: (forall s. Gen s -> ST s a) }

-- | For allowing the Monadic syntax when using @Rand@                                                                                       
instance Monad Rand where
   return k = Rand (\ _ -> return k)
   Rand c1 >>= fc2 = Rand (\ g -> c1 g >>= \a -> unR (fc2 a) g)

-- | Lets a computation run in the Rand Monad                                                                                                
runRand :: Int -> Rand a -> a
runRand seed (Rand c) = runST (do gen <- initialize $ singleton $ fromIntegral seed
                                  c gen)
                               --   x <- c gen
                              --    seed' <- save gen
                              --    return (x,seed'))

-- | Provides a random @Float@ in @[0..1)@                                                                                                   
rnd :: Rand Float
rnd = Rand uniform

rndRI :: (Int, Int) -> Rand Int
rndRI (lo, hi) = do
   t <- rnd
   return $ lo + round (t * fromIntegral (hi - lo))

-- | Provides a random @Float@ in the specified range (left inclusive, right exclusive)                                                      
rndR :: (Float, Float) -> Rand Float
rndR (lo, hi) = do
   t <- rnd
   return ((1.0 - t) * lo + t * hi)
