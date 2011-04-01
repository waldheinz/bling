
module Filter (
   
   -- * Creating Pixel Filters
   
   Filter, mkBoxFilter, mkSincFilter, mkTriangleFilter, mkMitchellFilter,
   
   -- * Evaluating Pixel Filters
   
   filterSample, filterWidth, filterHeight
   
   ) where
   
import Spectrum

import Data.Vector.Unboxed

-- | the size of tabulated pixel filters
tableSize :: Int
tableSize = 16

-- | a pixel filtering function
data Filter
   = Box Float 
   | Sinc {
      _xw :: Float,
      _yw :: Float,
      _tau :: Float
      }
   | Table Float Float (Vector Float) String

instance Show Filter where
   show (Box r) = "Box " Prelude.++ show r
   show (Sinc _ _ _) = "Sinc"
   show (Table w h _ n) = n Prelude.++ " {w=" Prelude.++ show w Prelude.++
         ", h=" Prelude.++ show h Prelude.++ "}"
   
-- | creates a box filter
mkBoxFilter :: Float -> Filter
mkBoxFilter = Box 

-- | creates a Sinc filter
mkSincFilter :: Float -> Float -> Float -> Filter
--mkSincFilter = Sinc
mkSincFilter w h t = mkTableFitler w h f "Sinc" where
   f (x, y) = (sinc1D t x * iw) * (sinc1D t y * ih)
   iw = 1 / w
   ih = 1 / h

-- | creates a triangle filter
mkTriangleFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Filter -- ^ the filter function

mkTriangleFilter w h = mkTableFitler w h f "Triangle" where
   f (x, y) = max 0 (w - abs x) * max 0 (h - abs y)

-- | creates a mitchell filter
mkMitchellFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Float -- ^ the Mitchell "B" parameter
   -> Float -- ^ the Mitchell "C" parameter
   -> Filter -- ^ the created filter
   
mkMitchellFilter w h b c = mkTableFitler w h f "Mitchell" where
   f (px, py) = (m1d (px * iw)) * (m1d (py * ih))
   (iw, ih) = (1 / w, 1 / h)
   m1d x' = y where
      x = abs (2 * x')
      y = if x > 1
             then (((-b) - 6*c) * x*x*x + (6*b + 30*c) * x*x +
                    ((-12)*b - 48*c) * x + (8*b + 24*c)) * (1/6)
             else ((12 - 9*b - 6*c) * x*x*x +
                   ((-18) + 12*b + 6*c) * x*x +
                    (6 - 2*b)) * (1/6)

mkTableFitler :: Float -> Float -> ((Float, Float) -> Float) -> String -> Filter
mkTableFitler w h f n = Table w h vs n where
   vs = fromList (Prelude.map f ps)
   ps = tablePositions w h

-- | finds the positions where the filter function has to be evaluated
-- to create the filter table
tablePositions :: (Fractional b) => b -> b -> [(b, b)]
tablePositions w h = Prelude.map f is where
   f (x, y) = ((x + 0.5) * w1, (y + 0.5) * h1)
   is = [(fromIntegral x, fromIntegral y) | y <- is', x <- is']
   is' = [0..tableSize-1]
   w1 = w / fromIntegral tableSize
   h1 = h / fromIntegral tableSize

-- | computes the with in pixels of a given @Filter@
filterWidth :: Filter -> Float
filterWidth (Box s) = s
filterWidth (Sinc w _ _) = w
filterWidth (Table w _ _ _) = w

-- | computes the height in pixels of a given @Filter@
filterHeight :: Filter -> Float
filterHeight (Box s) = s
filterHeight (Sinc _ h _) = h
filterHeight (Table _ h _ _) = h

-- | applies the given pixel @Filter@ to the @ImageSample@
filterSample :: Filter -> ImageSample -> [(Int, Int, WeightedSpectrum)]
{-# INLINE filterSample #-}
filterSample (Box _) (ImageSample x y ws) = [(floor x, floor y, ws)]
filterSample (Sinc xw yw tau) smp = sincFilter xw yw tau smp
filterSample (Table w h t _) s = tableFilter w h t s

tableFilter
   :: Float -> Float
   -> Vector Float 
   -> ImageSample
   -> [(Int, Int, WeightedSpectrum)]
{-# INLINE tableFilter #-}
tableFilter fw fh tbl (ImageSample ix iy (wt, s)) = go where
   (dx, dy) = (ix - 0.5, iy - 0.5)
   x0 = ceiling (dx - fw)
   x1 = floor (dx + fw)
   y0 = ceiling (dy - fh)
   y1 = floor (dy + fh)
   fx = (1 / fw) * fromIntegral tableSize
   fy = (1 / fh) * fromIntegral tableSize
   ifx = fromList [min (tableSize-1) (floor (abs ((x - dx) * fx)))
      | x <- Prelude.map fromIntegral [x0 .. x1]] :: Vector Int
   ify = fromList [min (tableSize-1) (floor (abs ((y - dy) * fy)))
      | y <- Prelude.map fromIntegral [y0 .. y1]] :: Vector Int
   o x y = ((unsafeIndex ify (y-y0)) * tableSize) + (unsafeIndex ifx (x - x0))
   w x y = wt * (unsafeIndex tbl (o x y))
   go = [(x, y, (wt * w x y, sScale s (w x y))) | y <- [y0..y1], x <- [x0..x1]]
   
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

