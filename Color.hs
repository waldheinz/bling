module Color where

import Math

-- | A Spectrum of colours.
type Spectrum = Vector -- RGB for now

-- | A "black" @Spectrum@ (no transmittance or emission) at all wavelengths
black :: Spectrum
black = (0, 0, 0)

-- | A "white" @Spectrum@ (full transmission at any wavelength).
white :: Spectrum
white = (1, 1, 1)

-- instance Num Spectrum where
--   Spectrum r1 g1 b1 + Spectrum r2 g2 b2 = 

-- | Decides if a @Spectrum@ is black (within an epsilon value).
isBlack :: Spectrum -> Bool
isBlack (r, g, b) = r < epsilon && g < epsilon && b < epsilon

sScale :: Spectrum -> Spectrum -> Spectrum
sScale (a, b, c) (d, e, f) = (a*d, b*e, c*f)

spectrumNaN :: Spectrum -> Bool
spectrumNaN (r, g, b) = (isNaN r) || (isNaN g) || (isNaN b)

pow :: Spectrum -> Spectrum -> Spectrum
pow (c1, c2, c3) (e1, e2, e3) = (p' c1 e1, p' c2 e2, p' c3 e3) where
   p' :: Float -> Float -> Float
   p' c e
      | c > 0 = c ** e
      | otherwise = 0
      
type WeightedSpectrum = (Float, Spectrum)

