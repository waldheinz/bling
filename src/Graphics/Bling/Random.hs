{-# LANGUAGE RankNTypes #-}

module Graphics.Bling.Random (

   CameraSample(..),
   
   -- * managing the random number generator
   
   Rand, Rand2D, runRand, runRandST, mkRndGen,
      
   -- * generating random values
   
   rnd2D, rnd, stratified2D
   ) where

import Control.Monad (liftM)
import Control.Monad.ST
import Data.Vector.Unboxed
import System.Random.MWC

type Rand2D = (Float, Float)

-- | Marks a computation that requires random values
newtype Rand a = Rand {
   unR :: forall s. Gen s -> ST s a
   }

{-# INLINE unR #-}

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

almostOne :: Float
almostOne = 0.9999999403953552 -- 0x1.fffffep-1

-- | generates startified samples in two dimensions
stratified2D
   :: Int -- ^ number of samples in first dimension
   -> Int -- ^ number of samples in second dimension
   -> Rand [Rand2D]

stratified2D nu nv = do
   js <- rndList2D (nu * nv)
   return $ Prelude.zipWith j uvs js where
      (du, dv) = (1 / fromIntegral nu, 1 / fromIntegral nv)
      j (u, v) (ju, jv) = (min almostOne ((u+ju)*du), min almostOne ((v+jv)*dv))
      uvs = [(fromIntegral u, fromIntegral v) | u <- [0..(nu-1)], v <- [0..(nv-1)]]

data CameraSample = CameraSample {
   imageX :: Float,
   imageY :: Float,
   lens :: Rand2D
   }
   