
module Graphics.Bling.Types (
   Printable(..), Flt
   ) where

import Text.PrettyPrint

class Printable a where
   
   prettyPrint :: a -> Doc
   

type Flt = Float
