
module Graphics.Bling.Primitive.Fractal (
   
) where

import Graphics.Bling.Math

-- | a Quaternion
data Quaternion = Quaternion
   { real :: ! Flt
   , imag :: Vector
   } deriving (Show, Eq)

qzero :: Quaternion
qzero = Quaternion 1 $ vpromote 0

qdot :: Quaternion -> Quaternion -> Flt
qdot q r = real q * real r + (imag q `dot` imag r)

qadd :: Quaternion -> Quaternion -> Quaternion
qadd q r = Quaternion r' i' where
   r' = real q + real r
   i' = imag q + imag r

qmul :: Quaternion -> Quaternion -> Quaternion
qmul q r = Quaternion r' i' where
   r' = r1 * r2 - imag q `dot` imag r
   (r1, r2) = (real q, real r)
   i' = mkV (x4, y4, z4)
   x4 = x3 + r1 * x2 + r2 * x1
   y4 = y3 + r1 * y2 + r2 * y1
   z4 = z3 + r1 * z2 + r2 * z1
   (Vector x1 y1 z1) = imag q
   (Vector x2 y2 z2) = imag r
   (Vector x3 y3 z3) = imag q `cross` imag r
   