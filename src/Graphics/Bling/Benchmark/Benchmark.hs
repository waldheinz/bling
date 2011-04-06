
module Main where

import Control.Monad.ST (stToIO)
import Criterion.Main

import Graphics.Bling.Filter
import Graphics.Bling.Image
import Graphics.Bling.Spectrum

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--
-- filter benchmarks
--

benchFilter :: Filter -> Benchmark
benchFilter f = bench (show f) (nf fn smp) where
   fn = filterSample f
   smp = ImageSample 10 10 (1, fromRGB (0.9, 0.9, 0.9))

filters :: [Filter]
filters = [
   mkBoxFilter,
   mkSincFilter 4 4 3,
   mkTableFilter (mkSincFilter 4 4 3),
   mkTriangleFilter 3 3,
   mkMitchellFilter 3 3 (1/3) (1/3) ]

--
-- image benchmarks
--

benchAddPixel :: Benchmark
benchAddPixel = bench "addSample" (nfIO fn) where
   -- TODO: this mainly benchmarks image creation
   fn = stToIO $ do
      img <- (mkImage mkBoxFilter 50 50)
      addSample img smp where
      
   smp = ImageSample 10 10 (1, fromRGB (0.9, 0.9, 0.9))
   
main :: IO ()
main = defaultMain [
   bgroup "image" [benchAddPixel],
   bgroup "filter" (map benchFilter filters)
   ]

