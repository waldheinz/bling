
module Graphics.Bling.Primitive.Fractal (
   
   -- * Fractals
   
   mkJuliaQuat, Quaternion(..),
   
) where

import qualified Data.Vector as V

import Graphics.Bling.AABB
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection

--
-- Julia Fractal
--

-- data Fractal = Julia {-# UNPACK #-} !Quaternion {-# UNPACK #-} !Float {-# UNPACK #-} !Int

mkJuliaQuat :: Material -> Quaternion -> Float -> Int -> Primitive
mkJuliaQuat mat q e mi = prim where
   prim = Primitive inter inters wb Nothing const
   wb = AABB (mkPoint' n n n) $ mkPoint' p p p where
      (n, p) = (-juliaRadius, juliaRadius)
      
   inters r = maybe False (const True) t where
      t = traverseJulia r q mi e
   
   inter r =
      traverseJulia r q mi e >>= \(d, o) ->
         Just $ mkIntersection d (e * 2) (mkDg' o $ normalJulia o q mi e) prim mat
   
prepare :: Ray -> Maybe Float
prepare (Ray ro rd rmin rmax)
   | c <= 0 = Just rmin -- start inside sphere
   | otherwise = solveQuadric a b c >>= cb
   where
         cb (t0, t1) -- check with ray bounds
            | t0 > rmax || t1 < rmin = Nothing
            | otherwise = Just t0

         c = sqLen ro - juliaRadius2
         a = sqLen rd
         b = 2 * (rd `dot` ro)

-- | the radius of the sphere where the Julia Quaternion lives
juliaRadius :: Float
juliaRadius = sqrt juliaRadius2

-- | often we need @juliaRadius@ squared
juliaRadius2 :: Float
juliaRadius2 = 3

traverseJulia
   :: Ray
   -> Quaternion
   -> Int
   -> Float
   -> Maybe (Float, Point)
   
traverseJulia r' c mi e = {-# SCC "traverseJulia" #-} prepare r >>= go where
   r = normalizeRay r'
   go d
      | sqLen o > juliaRadius2 + e = Nothing
      | dist < e = if onRay r d then Just (d, o) else Nothing
      | otherwise = go (d + dist)
      where
         dist = (0.5 * nz * log nz) / qlen zp
         nz = qlen z
         o = rayAt r d
         (z, zp) = iter (qpromote o) c mi
   
qpromote :: Point -> Quaternion
qpromote (Vector x y z) = Quaternion x $ mkV (y, z, 0)

normalJulia :: Point -> Quaternion -> Int -> Float -> Normal
normalJulia p c mi e = {-# SCC "normalJulia" #-} normalize v where
   v = mkV (gx, gy, gz)
   (gx, gy, gz) = (
      qlen (V.unsafeIndex v' 1) - qlen (V.unsafeIndex v' 0),
      qlen (V.unsafeIndex v' 3) - qlen (V.unsafeIndex v' 2),
      qlen (V.unsafeIndex v' 5) - qlen (V.unsafeIndex v' 4))
      
   qp = qpromote p
   (dx, gx1, gx2) = (qpromote $ mkV (e, 0, 0), qp `qsub` dx, qp `qadd` dx)
   (dy, gy1, gy2) = (qpromote $ mkV (0, e, 0), qp `qsub` dy, qp `qadd` dy)
   (dz, gz1, gz2) = (qpromote $ mkV (0, 0, e), qp `qsub` dz, qp `qadd` dz)
   v' = {-# SCC "normalJulia.iter'" #-} iter' (V.fromList [gx1, gx2, gy1, gy2, gz1, gz2]) c mi
   
-- | iterates several @Quaternion@s together
iter'
   :: V.Vector Quaternion -- ^ the quaternions to iterate
   -> Quaternion -- ^ the c value
   -> Int -- ^ the number of iterations
   -> V.Vector Quaternion
   
iter' qs c n = iterate (V.map $ qadd c . qsq) qs !! n

-- | if the magnitude of the quaternion exceeds this value it is considered
-- to diverge
escapeThreashold :: Float
escapeThreashold = 4

iter
   :: Quaternion -- ^ the quaternion to iterate
   -> Quaternion -- ^ the @c@ value to add in each step
   -> Int -- ^ the maximum number of iterations
   -> (Quaternion, Quaternion) -- ^ the result and it's derivate

iter qi c mi = go qi qzero mi where
   go q qp i
      | i == 0 = (q', qp')
      | qlen q > escapeThreashold = (q', qp')
      | otherwise = go q' qp' (i-1) where
         q' = qsq q `qadd` c
         qp' = (q `qmul` qp) `qscale` 2
         
-- | a Quaternion
data Quaternion = Quaternion
   { real :: {-# UNPACK #-} ! Float
   , imag :: {-# UNPACK #-} ! Vector
   } deriving (Show, Eq)

qzero :: Quaternion
qzero = Quaternion 1 $ vpromote 0

qlen :: Quaternion -> Float
qlen (Quaternion r (Vector i j k)) = sqrt $ r*r + i*i + j*j + k*k

qscale :: Quaternion -> Float -> Quaternion
qscale (Quaternion r i) s = Quaternion (r*s) $ vpromote s * i

qadd :: Quaternion -> Quaternion -> Quaternion
qadd q r = Quaternion (real q + real r) (imag q + imag r)

qsub :: Quaternion -> Quaternion -> Quaternion
qsub q r = Quaternion (real q - real r) (imag q - imag r)

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

