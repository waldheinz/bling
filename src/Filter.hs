
module Filter (
   -- * Creating Pixel Filters
   
   Filter, mkBoxFilter, mkSincFilter,
   
   -- * Evaluating Pixel Filters
   
   filterSample
   
   ) where
   
import Spectrum
   
-- | a pixel filtering function
data Filter
   = Box -- ^ a simple box filter
   | Sinc {
      _xw :: Float,
      _yw :: Float,
      _tau :: Float
      }
   deriving (Show)

-- | creates a box filter
mkBoxFilter :: Filter
mkBoxFilter = Box

-- | creates a Sinc filter
mkSincFilter :: Float -> Float -> Float -> Filter
mkSincFilter = Sinc

-- | applies the given pixel @Filter@ to the @ImageSample@
filterSample :: Filter -> ImageSample -> [(Int, Int, WeightedSpectrum)]
filterSample Box (ImageSample x y ws) = [(floor x, floor y, ws)]
filterSample (Sinc xw yw tau) smp = sincFilter xw yw tau smp

sincFilter :: Float -> Float -> Float -> ImageSample -> [(Int, Int, WeightedSpectrum)]
sincFilter xw yw tau (ImageSample px py (sw, ss)) = [(x, y, (sw * ev x y, sScale ss (ev x y))) | (x, y) <- pixels] where
   pixels = [(x :: Int, y :: Int) | y <- [y0..y1], x <- [x0..x1]]
   x0 = ceiling (px - xw)
   x1 = floor (px + xw)
   y0 = ceiling (py - yw)
   y1 = floor (py + yw)
   ev x y = sinc1D tau x' * sinc1D tau y' where
      x' = (fromIntegral x - px + 0.5) / xw
      y' = (fromIntegral y - py + 0.5) / yw

sinc1D :: Float -> Float -> Float
sinc1D tau x
   | x > 1 = 0
   | x == 0 = 1
   | otherwise = sinc * lanczos where
      x' = x * pi
      sinc = sin (x' * tau) / (x' * tau)
      lanczos = sin x' / x'

