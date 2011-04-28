
module Graphics.Bling.Sampler.Stratified (
   StratifiedSampler, mkStratifiedSampler
   ) where

import Graphics.Bling.Random
import Graphics.Bling.Sampling

data StratifiedSampler = SS
   { _nu :: Int
   , _nv :: Int
   }
   
mkStratifiedSampler :: Int -> Int -> StratifiedSampler
mkStratifiedSampler = SS

instance Sampler StratifiedSampler where
   samples (SS nu nv) w = undefined

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
