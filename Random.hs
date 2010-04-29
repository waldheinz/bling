
-- | Provides a monad for computations requiring random numbers
module Random(Rand, Rand2D, fromRand, runRand, rndR, rndRI, rnd) where

import System.Random

type Rand2D = (Float, Float)

-- | Marks a computation that requires random values
data Rand a = Rand (StdGen -> (a, StdGen))

-- | For allowing the Monadic syntax when using @Rand@
instance Monad Rand where
   return k = Rand (\s -> (k, s))
   Rand c1 >>= fc2 = Rand (\s0 ->  let 
                                       (r,s1) = c1 s0 
                                       Rand c2 = fc2 r in
                                       c2 s1)

-- | Lets a computation run in the Rand Monad
runRand :: StdGen -> Rand a -> (a, StdGen)
runRand rng (Rand c) = c rng

-- | Extracts the result of a stochastic computation from the @Random@ Monad,
-- the rng state gets lost.
fromRand :: (a, StdGen) -> a
fromRand (a, _) = a

-- | Provides a random @Float@ in @[0..1)@
rnd :: Rand Float
rnd = Rand (random)

rndRI :: (Int, Int) -> Rand Int
rndRI range = Rand (randomR range)

-- | Provides a random @Float@ in the specified range (left inclusive, right exclusive)
rndR :: (Float, Float) -> Rand Float
rndR (lo, hi) = do
   t <- rnd
   return ((1.0 - t) * lo + t * hi)
-- rndR range = Rand (randomR range)

--instance Show (Rand a) where
--   show (Rand a) = fromRand $ (runRand (mkStdGen 0) a)
