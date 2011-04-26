
module Graphics.Bling.Sampler.Random (

   -- * Creating a @RandomSampler@
   
   mkRandomSampler
   
   ) where

import Data.Vector.Generic

import Graphics.Bling.Random as R
import Graphics.Bling.Sampling

data RandomSampler = RS {
   _spp :: Int
   }

mkRandomSampler :: Int -> RandomSampler
mkRandomSampler = RS

instance Sampler RandomSampler where
   
   samples (RS spp) w = Prelude.mapM smp ws where
      ws = Prelude.concatMap (Prelude.replicate spp) (coverWindow w)
      smp :: (Int, Int) -> R.Rand Sample
      smp (ix, iy) = do
         ox <- R.rnd
         oy <- R.rnd
         luv <- R.rnd2D
         return $ Sample
            ((fromIntegral ix) + ox)
            ((fromIntegral iy) + oy)
            luv
            empty empty
      