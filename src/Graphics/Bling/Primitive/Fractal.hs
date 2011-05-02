
module Graphics.Bling.Primitive.Fractal (
   
) where

import Graphics.Bling.Math

-- | a Quaternion
data Quaternion = Quaternion
   { real :: ! Flt
   , imag :: Vector
   }

qdot :: Quaternion -> Quaternion -> Flt
qdot q r = real q * real r + (imag q `dot` imag r)

