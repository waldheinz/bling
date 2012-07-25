
module Graphics.Bling.Filter (
   
   -- * Creating Pixel Filters
   
   Filter, mkBoxFilter, mkSincFilter, mkTriangleFilter, mkMitchellFilter,
   mkTableFilter,
   
   -- * Evaluating Pixel Filters
   
   filterSample, filterWidth, filterHeight, filterSize
   
   ) where

import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as UV

import Graphics.Bling.Spectrum

-- | the size of tabulated pixel filters
tableSize :: Int
tableSize = 16

-- | a pixel filtering function
data Filter
   = Box 
   | Sinc {
      _xw :: Float,
      _yw :: Float,
      _tau :: Float
      }
   | Mitchell {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
   | Triangle Float Float
   | Table {-# UNPACK #-} !Float {-# UNPACK #-} !Float !(UV.Vector Float) String

instance Show Filter where
   show Box = "Box"
   show (Mitchell w h b c) = "Mitchell" Prelude.++
      " {w=" Prelude.++ show w Prelude.++
      ", h=" Prelude.++ show h Prelude.++
      ", b=" Prelude.++ show b Prelude.++
      ", c=" Prelude.++ show c Prelude.++ "}"
   show (Sinc _ _ _) = "Sinc"
   show (Table _ _ _ n) = "Table" Prelude.++
      " {orig=" Prelude.++ show n Prelude.++ "}"
   show (Triangle w h) = "Triangle" Prelude.++
         " {w=" Prelude.++ show w Prelude.++
         ", h=" Prelude.++ show h Prelude.++ "}"
         
-- | creates a box filter
mkBoxFilter :: Filter
mkBoxFilter = Box 

-- | creates a Sinc filter
mkSincFilter :: Float -> Float -> Float -> Filter
mkSincFilter = Sinc

-- | creates a triangle filter
mkTriangleFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Filter -- ^ the filter function

mkTriangleFilter = Triangle

-- | creates a mitchell filter
mkMitchellFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Float -- ^ the Mitchell "B" parameter
   -> Float -- ^ the Mitchell "C" parameter
   -> Filter -- ^ the created filter
   
mkMitchellFilter = Mitchell

mkTableFilter :: Filter -> Filter
mkTableFilter f = Table w h vs n where
   w = filterWidth f
   h = filterHeight f
   n = show f
   vs = UV.fromList (map (uncurry (evalFilter f)) ps)
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

filterSize :: Filter -> (Float, Float)
filterSize f = (filterWidth f, filterHeight f)

-- | computes the width in pixels of a given @Filter@
filterWidth :: Filter -> Float
filterWidth Box = 0.5
filterWidth (Mitchell w _ _ _) = w
filterWidth (Sinc w _ _) = w
filterWidth (Table w _ _ _) = w
filterWidth (Triangle w _) = w

-- | computes the height in pixels of a given @Filter@
filterHeight :: Filter -> Float
filterHeight Box = 0.5
filterHeight (Mitchell _ h _ _) = h
filterHeight (Sinc _ h _) = h
filterHeight (Table _ h _ _) = h
filterHeight (Triangle _ h) = h

-- | applies the given pixel @Filter@ to the @ImageSample@
filterSample :: (PrimMonad m) => Filter -> ImageSample -> ((Int, Int, WeightedSpectrum) -> m ()) -> m ()
{-# INLINE filterSample #-}
filterSample Box (ImageSample x y ws) fun = fun (floor x, floor y, ws)
filterSample (Table w h t _) s _ = error "table not implemented " -- tableFilter w h t s
filterSample f (ImageSample ix iy (sw, s)) fun = mapM_ fun go where
   go = [(x, y, (sw * w x y, sScale s (w x y))) | y <- [y0..y1], x <- [x0..x1]]
   (dx, dy) = (ix - 0.5, iy - 0.5)
   x0 = ceiling (dx - fw)
   x1 = floor (dx + fw)
   y0 = ceiling (dy - fh)
   y1 = floor (dy + fh)
   w x y = evalFilter f (fromIntegral x - ix) (fromIntegral y - iy)
   fw = filterWidth f
   fh = filterHeight f

evalFilter :: Filter -> Float -> Float -> Float
{-# INLINE evalFilter #-}
evalFilter (Mitchell w h b c) px py = m1d (px * iw) * m1d (py * ih) where
   (iw, ih) = (1 / w, 1 / h)
   m1d x' = y where
      x = abs (2 * x')
      y = if x > 1
             then (((-b) - 6*c) * x*x*x + (6*b + 30*c) * x*x +
                    ((-12)*b - 48*c) * x + (8*b + 24*c)) * (1/6)
             else ((12 - 9*b - 6*c) * x*x*x +
                   ((-18) + 12*b + 6*c) * x*x +
                    (6 - 2*b)) * (1/6)
                    
evalFilter (Sinc _ _ tau) px py = sinc1D px * sinc1D py where
   sinc1D x
      | x > 1 = 0
      | x == 0 = 1
      | otherwise = sinc * lanczos where
         x' = x * pi
         sinc = sin (x' * tau) / (x' * tau)
         lanczos = sin x' / x'
         
evalFilter (Triangle w h) x y = f (x, y) where
   f (px, py) = max 0 (w - abs px) * max 0 (h - abs py)

evalFilter f _ _ =
   error ("evalFilter for " Prelude.++ show f Prelude.++ " called")

tableFilter
   :: Float -> Float
   -> UV.Vector Float 
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
   ifx = UV.fromList [min (tableSize-1) (floor (abs ((x - dx) * fx)))
      | x <- map fromIntegral [x0 .. x1]] :: UV.Vector Int
   ify = UV.fromList [min (tableSize-1) (floor (abs ((y - dy) * fy)))
      | y <- map fromIntegral [y0 .. y1]] :: UV.Vector Int
   o x y = (UV.unsafeIndex ify (y-y0) * tableSize) + UV.unsafeIndex ifx (x - x0)
   w x y = wt * UV.unsafeIndex tbl (o x y)
   go = [(x, y, (wt * w x y, sScale s (w x y))) | y <- [y0..y1], x <- [x0..x1]]
   
