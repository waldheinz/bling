
module Graphics.Bling.Sampler.Stratified (
   StratifiedSampler, mkStratifiedSampler
   ) where
{-
import Control.Monad (liftM, replicateM, forM_)
import Control.Monad.Primitive
import Data.List (transpose, zipWith4)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import System.Random
import qualified System.Random.Shuffle as S
import System.Random.MWC

import Graphics.Bling.Math
import Graphics.Bling.Random
import Graphics.Bling.Sampling
-}
data StratifiedSampler = SS
   { _nu :: Int
   , _nv :: Int
   }
   
mkStratifiedSampler :: Int -> Int -> StratifiedSampler
mkStratifiedSampler = SS
{-
instance Sampler StratifiedSampler where
   samples (SS nu nv) w n1d n2d = concat `liftM` mapM (pixel nu nv n1d n2d) (coverWindow w)

-- | creates stratified samples for one pixel
pixel :: Int -> Int -> Int -> Int -> (Int, Int) -> Rand [Sample]
pixel nu nv n1d n2d (px, py) = do
   lens <- stratified2D nu nv >>= shuffle (nu*nv)
   ps <- stratified2D nu nv
   r1d <- mk1D (nu*nv) n1d
   r2d <- mk2D nu nv n2d
   
   return $ mkSamples (shiftToPixel px py ps) lens r1d r2d 

mkSamples
   :: [(Flt, Flt)] -- ^ pixel coordinates
   -> [Rand2D] -- ^ lens coordinates
   -> [V.Vector Flt] -- ^ 1d samples
   -> [V.Vector Rand2D] -- ^ 2d samples
   -> [Sample]
mkSamples = zipWith4 go where
   go (px, py) lens r1d r2d = Sample px py lens r1d r2d

-- | shuffles a list
shuffle
   :: Int -- ^ the length of the list
   -> [a] -- ^ the list to shuffle
   -> Rand [a]
shuffle xl xs = do
   seed <- rndInt
   return $ {-# SCC "shuffle'" #-} S.shuffle' xs xl $ mkStdGen seed

   
vectorize :: (V.Unbox a) => [[a]] -> [V.Vector a]
vectorize xs = map V.fromList $ transpose xs

mk1D :: Int -> Int -> Rand [V.Vector Flt]
mk1D nu n = do
   vals <- replicateM n $ (stratified1D nu) >>= shuffle nu
   return $ vectorize vals

mk2D :: Int -> Int -> Int -> Rand [V.Vector Rand2D]
mk2D nu nv n = do
   vals <- replicateM n $ (stratified2D nu nv) >>= shuffle (nu*nv)
   return $ vectorize vals

almostOne :: Float
almostOne = 0.9999999403953552 -- 0x1.fffffep-1

stratified1D
   :: Int
   -> Rand [Flt]
stratified1D n = do
   js <- rndList n
   return $ Prelude.zipWith j us js where
      du = 1 / fromIntegral n
      j u ju = min almostOne ((u+ju)*du)
      us = [fromIntegral u | u <- [0..(n-1)]]
      
-- | generates stratified samples in two dimensions
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
         -}