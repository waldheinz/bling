
module Graphics.Bling.Primitive.Fractal (
   
   -- * Fractals
   
   Fractal, mkJuliaQuat, Quaternion(..),
   
   -- * The Fractal Primitive
   
   FractalPrim, mkFractalPrim
   
) where

import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection

data Fractal = Julia
   { juliaC    :: Quaternion
   , epsilon   :: Flt
   , maxIt     :: Int
   }
   
mkJuliaQuat :: Quaternion -> Flt -> Int -> Fractal
mkJuliaQuat = Julia

data FractalPrim = FP
   { fractal   :: Fractal
   , material  :: Material
   }

mkFractalPrim :: Fractal -> Material -> FractalPrim
mkFractalPrim = FP
   
instance Primitive FractalPrim where
   intersect p@(FP (Julia mu e mi) mat) ray =
      intersectJulia ray mu mi e >>= \(d, dg) ->
         Just $ Intersection d dg (mkAnyPrim p) mat


intersectJulia
   :: Ray
   -> Quaternion
   -> Int
   -> Flt
   -> Maybe (Flt, DifferentialGeometry)

intersectJulia (Ray ro rd rmin rmax) c md e = go ro where
   go o
      | sqLen o > boundingRadius2 = Nothing
      | d < e = Just $ (d, mkDg o $ normalJulia o c md)
      | otherwise = go $ o + smul d rd
      where
         d = 0.5 * nz * log nz / qlen zp
         nz = qlen z
         (z, zp) = iter (qpromote o) c md
         
qpromote :: Point -> Quaternion
qpromote (Vector x y z) = Quaternion x $ mkV (y, z, 0)

normalJulia :: Point -> Quaternion -> Int -> Normal
normalJulia p c mi = normalize $ mkV (gx, gy, gz) where
   (gx, gy, gz) = (qlen gx2' - qlen gx1', qlen gy2' - qlen gy1', qlen gz2' - qlen gz1')
   qp = qpromote p
   (dx, gx1, gx2) = (qpromote $ mkV (delta, 0, 0), qp `qsub` dx, qp `qadd` dx)
   (dy, gy1, gy2) = (qpromote $ mkV (0, delta, 0), qp `qsub` dy, qp `qadd` dy)
   (dz, gz1, gz2) = (qpromote $ mkV (0, 0, delta), qp `qsub` dz, qp `qadd` dz)
   v' = iter' [gx1, gx2, gy1, gy2, gz1, gz2] c mi
   (gx1':gx2':gy1':gy2':gz1':gz2':[]) = v'
   
-- | iterates several @Quaternion@s together
iter'
   :: [Quaternion] -- ^ the quaternions to iterate
   -> Quaternion -- ^ the c value
   -> Int -- ^ the number of iterations
   -> [Quaternion]
   
iter' qs _ 0 = qs
iter' qs c n = iter' qs' c (n-1) where
   qs' = map (qadd c) $ map qsq qs

boundingRadius2 :: Flt
boundingRadius2 = 3

delta :: Flt
delta = 1e-4

-- | if the magnitude of the quaternion exceeds this value it is considered
-- to diverge
escapeThreashold :: Flt
escapeThreashold = 4

iter
   :: Quaternion -- ^ the quaternion to iterate
   -> Quaternion -- ^ the @c@ value to add in each step
   -> Int -- ^ the maximum number of iterations
   -> (Quaternion, Quaternion) -- ^ the result and it's derivate

iter qi c md = go qi qzero md where
   go q qp d
      | d == 0 = (q, qp)
      | q `qdot` q > escapeThreashold = (q, qp)
      | otherwise = go q' qp' (d-1) where
         q' = qsq q `qadd` c
         qp' = q `qmul` qp `qscale` 2
         
-- | a Quaternion
data Quaternion = Quaternion
   { real :: {-# UNPACK #-} ! Flt
   , imag :: {-# UNPACK #-} ! Vector
   } deriving (Show, Eq)

qzero :: Quaternion
qzero = Quaternion 1 $ vpromote 0

qlen :: Quaternion -> Flt
qlen (Quaternion r (Vector i j k)) = sqrt $ r*r + i*i + j*j + k*k

qscale :: Quaternion -> Flt -> Quaternion
qscale (Quaternion r i) s = Quaternion (r*s) $ vpromote s * i

qdot :: Quaternion -> Quaternion -> Flt
qdot q r = real q * real r + (imag q `dot` imag r)

qadd :: Quaternion -> Quaternion -> Quaternion
qadd q r = Quaternion r' i' where
   r' = real q + real r
   i' = imag q + imag r

qsub :: Quaternion -> Quaternion -> Quaternion
qsub q r = Quaternion r' i' where
   r' = real q - real r
   i' = imag q - imag r

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

-- | computes the square of a @Quaternion@
qsq :: Quaternion -> Quaternion
qsq (Quaternion r i) = Quaternion r' i' where
   r' = r * r - i `dot` i
   i' = vpromote (2 * r) * i

