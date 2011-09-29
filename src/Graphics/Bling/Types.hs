
module Graphics.Bling.Types (
   Printable(..), Flt, PixelPos, PixelSize
   ) where

import Text.PrettyPrint

class Printable a where
   
   prettyPrint :: a -> Doc
   

type Flt = Float

-- | a pixel position, given in it's (x, y) coordinates
type PixelPos = (Int, Int)

-- | the size of an image (map), given in (width, height) in pixels
type PixelSize = (Int, Int)
