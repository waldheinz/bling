
module Graphics.Bling.Sampler.Stratified (
   StratifiedSampler, mkStratifiedSampler
   ) where

import Control.Monad (liftM)
import qualified Data.Vector.Generic as V
import System.Random
import System.Random.Shuffle

import Graphics.Bling.Math
import Graphics.Bling.Random
import Graphics.Bling.Sampling

data StratifiedSampler = SS
   { _nu :: Int
   , _nv :: Int
   }
   
mkStratifiedSampler :: Int -> Int -> StratifiedSampler
mkStratifiedSampler = SS

instance Sampler StratifiedSampler where
   samples (SS nu nv) w = concat `liftM` mapM (pixel nu nv) (coverWindow w)

pixel :: Int -> Int -> (Int, Int) -> Rand [Sample]
pixel nu nv (px, py) = do
   ls <- stratified2D nu nv
   sls <- rndInt
   ps <- stratified2D nu nv
   r2d <- stratified2D nu nv
   s2d <- rndInt
   
   return $ mkSamples
      (shuffle' ls (nu*nv) $ mkStdGen sls)
      (shuffle' r2d (nu*nv) $ mkStdGen s2d)
      (shiftToPixel px py ps)

mkSamples :: [Rand2D] -> [Rand2D] -> [(Flt, Flt)] -> [Sample]
mkSamples = zipWith3 go where
   go n2d lens (px, py) = Sample px py lens (V.singleton n2d) V.empty

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
