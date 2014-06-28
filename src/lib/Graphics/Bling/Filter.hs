
module Graphics.Bling.Filter (
   
   -- * Pixel Filters
   Filter, mkBoxFilter, mkTriangleFilter, mkMitchellFilter, mkSincFilter,
   mkGaussFilter,
   
   filterSize, evalFilter
   
   ) where
   
data Filter
   = Box
   | Gauss {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
           {-# UNPACK #-} !Float {-# UNPACK #-} !Float -- w, h, expX, expY, alpha
   | Sinc {-# UNPACK #-} !Float {-# UNPACK #-} !Float
          {-# UNPACK #-} !Float {-# UNPACK #-} !Float
          {-# UNPACK #-} !Float -- xw, xy, invx, invy, tau
   | Mitchell {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
   | Triangle {-# UNPACK #-} !Float {-# UNPACK #-} !Float
   deriving (Show)
   
-- | creates a box filter
mkBoxFilter :: Filter
mkBoxFilter = Box 

mkGaussFilter :: Float -> Float -> Float -> Filter
mkGaussFilter xw yw alpha = Gauss xw yw (exp $ -alpha * xw * xw) (exp $ -alpha * yw * yw) alpha

-- | creates a Lanczos - Sinc filter
mkSincFilter
   :: Float -- ^ the filter extent's width
   -> Float -- ^ the filter extent's height
   -> Float -- ^ the tau parameter for the filter, controls how many
            --   cycles the sinc passes through before camped to zero
   -> Filter
mkSincFilter wx wy tau = Sinc wx wy (1 / wx) (1 / wy) tau

-- | creates a triangle filter
mkTriangleFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Filter

mkTriangleFilter = Triangle

-- | creates a mitchell filter
mkMitchellFilter
   :: Float -- ^ the width of the filter extent
   -> Float -- ^ the height of the filter extent
   -> Float -- ^ the Mitchell "B" parameter
   -> Float -- ^ the Mitchell "C" parameter
   -> Filter
   
mkMitchellFilter = Mitchell   

filterSize :: Filter -> (Float, Float)
filterSize (Box)              = (0.5, 0.5)
filterSize (Gauss w h _ _ _)  = (w, h)
filterSize (Sinc w h _ _ _)   = (w, h)
filterSize (Mitchell w h _ _) = (w, h)
filterSize (Triangle w h)     = (w, h)

evalFilter :: Filter -> Float -> Float -> Float
evalFilter Box _ _ = 1
evalFilter (Gauss _ _ ex ey a) x y = gaussian x ex * gaussian y ey where
   gaussian d expv = max 0 $ exp (-a * d * d) - expv
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

evalFilter (Sinc _ _ ix iy tau) px py = sinc1D (px * ix) * sinc1D (py * iy) where
   sinc1D x
      | abs x > 1 = 0
      | abs x < 1e-5 = 1
      | otherwise = sinc * lanczos where
         x' = abs x * pi
         lanczos = sin (x' * tau) / (x' * tau)
         sinc = sin x' / x'
         
evalFilter (Triangle w h) x y = f (x, y) where
   f (px, py) = max 0 (w - abs px) * max 0 (h - abs py)

