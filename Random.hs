module Random(Rand, fromRand, runRand, rndR, rnd) where

import System.Random

---
--- a Monad providing a PRNG
---

data Rand a = Rand (StdGen -> (a, StdGen))

instance Monad Rand where
   return k = Rand (\s -> (k, s))
   Rand c1 >>= fc2 = Rand (\s0 ->  let 
                                       (r,s1) = c1 s0 
                                       Rand c2 = fc2 r in
                                       c2 s1)

  -- | Lets a computation run in the Random Monad
runRand :: StdGen -> Rand a -> (a, StdGen)
runRand rng (Rand c) = c rng

  -- | Extracts the result of a stochastic computation out of the @Random@ Monad
fromRand :: (a, StdGen) -> a
fromRand (a, _) = a

  -- | Provides a random @Float@ in @[0..1]@
rnd :: Rand Float
rnd = Rand (randomR (0, 1::Float))

  -- | Provides a random @Float@ in the specified range
rndR :: Random a => (a, a) -> Rand a
rndR range = Rand (randomR range)
