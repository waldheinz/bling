
module Graphics.Bling.Types (
   Printable(..), Float, PixelPos, PixelSize,
   
   -- * Coordinate Systems and Conversions
   SphericalCoords(..), CartesianCoords(..),
   cartToSph, sphToCart
   
   ) where

import Text.PrettyPrint

class Printable a where
   
   prettyPrint :: a -> Doc
   
-- | a pixel position, given in it's (x, y) coordinates
type PixelPos = (Int, Int)

-- | the size of an image (map), given in (width, height) in pixels
type PixelSize = (Int, Int)

-- | spherical coordinates which are given in (phi [0..2pi), theta [0..pi))
newtype SphericalCoords = Spherical { unSpherical :: (Float, Float) }
                          deriving ( Eq, Show )

-- | cartesian coordinates which are given in (u [0..1), v [0..1))
newtype CartesianCoords =
  Cartesian { unCartesian :: (Float, Float) } deriving ( Eq, Show )

cartToSph :: CartesianCoords -> SphericalCoords
{-# INLINE cartToSph #-}
cartToSph (Cartesian (u, v)) = Spherical $ (u * 2 * pi, v * pi)

sphToCart :: SphericalCoords -> CartesianCoords
{-# INLINE sphToCart #-}
sphToCart (Spherical (phi, theta)) = Cartesian (u, v) where
   u = phi / (2 * pi)
   v = theta / pi
