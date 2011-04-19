
module Graphics.Bling.Sampler.Random (

   -- * Creating a @RandomSampler@
   
   mkRandomSampler
   
   ) where

import Graphics.Bling.Sampling

data RandomSampler = RandomSampler {
   
   }

mkRandomSampler :: SampleWindow -> RandomSampler
mkRandomSampler w = undefined


instance Sampler RandomSampler where
   