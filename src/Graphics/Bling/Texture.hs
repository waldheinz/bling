
module Graphics.Bling.Texture (
   -- * Texture Types
   
   Texture, SpectrumTexture,
   
   -- * Creating Textures
   constant, graphPaper
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Spectrum

-- | A @Texture@ transforms a @DifferentialGeomerty@ to some value
type Texture a = DifferentialGeometry -> a

type SpectrumTexture = Texture Spectrum

constant :: a -> Texture a
constant r _ = r

graphPaper :: Float -> Spectrum -> Spectrum -> SpectrumTexture
graphPaper lw p l (DifferentialGeometry (Vector x _ z) _)
   | x' < lo || z' < lo || x' > hi || z' > hi = l
   | otherwise = p
   where
         x' = abs x''
         z' = abs z''
         (_, x'') = properFraction x :: (Int, Float)
         (_, z'') = properFraction z :: (Int, Float)
         lo = lw / 2
         hi = 1.0 - lo
