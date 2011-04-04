
module Main where

import Criterion.Main

import Filter
import Spectrum

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



benchFilter :: Filter -> Benchmark
benchFilter f = bench (show f) (nf fn smp) where
   fn = filterSample f
   smp = ImageSample 10 10 (1, fromRGB (0.9, 0.9, 0.9))

filters :: [Filter]
filters = [
   mkBoxFilter 0.5,
   mkSincFilter 4 4 3,
   mkTriangleFilter 3 3,
   mkMitchellFilter 3 3 (1/3) (1/3)
   ]

main :: IO ()
main = defaultMain [bgroup "filter" (map benchFilter filters) ]

