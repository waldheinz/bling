
module Graphics.Bling.Examples.Spiral where

import Graphics.Bling.Edsl
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum

spiral = render $ do
   emit white
   shape (mkSphere 1) >>= add
   emit black
   return ()

main = spiral

