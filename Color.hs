
module Color (
   Spectrum(..), WeightedSpectrum, 
   white, black, 
   isBlack, sNaN,
   fromXyz, sScale, sPow) where

import Math

-- | A Spectrum of colours.
data Spectrum = Spectrum Float Float Float deriving (Show, Eq)

-- | A "black" @Spectrum@ (no transmittance or emission) at all wavelengths
black :: Spectrum
black = Spectrum 0 0 0

-- | A "white" @Spectrum@ (full transmission at any wavelength).
white :: Spectrum
white = Spectrum 1 1 1

-- (*) :: Spectrum -> Spectrum -> Spectrum

fromXyz :: (Float, Float, Float) -> Spectrum
fromXyz (x, y, z) = Spectrum x y z

instance Num Spectrum where
   Spectrum r1 g1 b1 + Spectrum r2 g2 b2 = Spectrum (r1+r2) (g1+g2) (b1+b2)
   Spectrum r1 g1 b1 - Spectrum r2 g2 b2 = Spectrum (r1-r2) (g1-g2) (b1-b2)
   Spectrum r1 g1 b1 * Spectrum r2 g2 b2 = Spectrum (r1*r2) (g1*g2) (b1*b2)
   abs (Spectrum r g b) = Spectrum (abs r) (abs g) (abs b)
   negate (Spectrum r g b) = Spectrum (-r) (-g) (-b)
   signum (Spectrum r g b) = Spectrum (signum r) (signum g) (signum b)
   fromInteger i = Spectrum i' i' i' where
      i' = fromIntegral i
   
-- | Decides if a @Spectrum@ is black (within an epsilon value).
isBlack :: Spectrum -> Bool
isBlack (Spectrum r g b) = r < epsilon && g < epsilon && b < epsilon

sScale :: Spectrum -> Float -> Spectrum
sScale (Spectrum a b c) f = Spectrum (a*f) (b*f) (c*f)

sNaN :: Spectrum -> Bool
sNaN (Spectrum r g b) = (isNaN r) || (isNaN g) || (isNaN b)

sPow :: Spectrum -> Spectrum -> Spectrum
sPow (Spectrum c1 c2 c3) (Spectrum e1 e2 e3) = Spectrum (p' c1 e1) (p' c2 e2) (p' c3 e3) where
   p' :: Float -> Float -> Float
   p' c e
      | c > 0 = c ** e
      | otherwise = 0
      
type WeightedSpectrum = (Float, Spectrum)

