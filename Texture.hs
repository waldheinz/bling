
module Texture where

import Debug.Trace

import Color
import Geometry

type SpectrumTexture = DifferentialGeometry -> Spectrum

constantSpectrum :: Spectrum -> SpectrumTexture
constantSpectrum r _ = r

graphPaper :: Float -> Spectrum -> Spectrum -> SpectrumTexture
graphPaper lw p l (DifferentialGeometry (x, _, z) _)
   | x' < lo || z' < lo || x' > hi || z' > hi = l
   | otherwise = p
   where
         x' = abs x''
         z' = abs z''
         (_, x'') = properFraction x
         (_, z'') = properFraction z
         lo = lw / 2
         hi = 1.0 - lo
