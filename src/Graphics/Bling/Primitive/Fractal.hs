
module Graphics.Bling.Primitive.Fractal (
   
   -- * Fractals
   
   Fractal, mkJuliaQuat, Quaternion(..),
   
   -- * The Fractal Primitive
   
   FractalPrim, mkFractalPrim, mkMengerSponge
   
) where

import Graphics.Bling.AABB
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.Geometry
import Graphics.Bling.Reflection
import Graphics.Bling.Shape

--
-- Julia Fractal
--

data Fractal = Julia {-# UNPACK #-} !Quaternion {-# UNPACK #-} !Flt {-# UNPACK #-} !Int
   
mkJuliaQuat :: Quaternion -> Flt -> Int -> Fractal
mkJuliaQuat = Julia

data FractalPrim
   = FP
      { _fractal  :: ! Fractal
      , _material :: ! Material
      }
   | Menger
      { _base     :: ! Shape
      , _mMat     :: ! Material
      , _level    :: ! Int
      , _trans    :: ! Transform
      }

mkFractalPrim :: Fractal -> Material -> FractalPrim
mkFractalPrim = FP

mkMengerSponge :: Shape -> Material -> Int -> Transform -> FractalPrim
mkMengerSponge = Menger

instance Primitive FractalPrim where
   flatten (Menger g m l t) = buildMenger g m l t
   flatten fp = [mkAnyPrim fp]
   
   worldBounds _ = AABB (mkPoint' n n n) $ mkPoint' p p p where
      (n, p) = (-juliaRadius, juliaRadius)
      
   intersects (FP (Julia q e mi) _) r = maybe False (const True) t where
      t = traverseJulia r q mi e
   
   intersects (Menger _ _ _ _) _ = error "Menger : unimplemented intersects"
   
   
   intersect p@(FP (Julia q e mi) m) r =
      traverseJulia r q mi e >>= \(d, o) ->
         Just $ Intersection d (e * 2) (mkDg' o $ normalJulia o q mi e) (mkAnyPrim p) m
   
   intersect (Menger _ _ _ _) _ = error "Menger : unimplemented intersects"

prepare :: Ray -> Maybe Flt
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
juliaRadius :: Flt
juliaRadius = sqrt juliaRadius2

-- | often we need @juliaRadius@ squared
juliaRadius2 :: Flt
juliaRadius2 = 3

traverseJulia
   :: Ray
   -> Quaternion
   -> Int
   -> Flt
   -> Maybe (Flt, Point)
   
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

normalJulia :: Point -> Quaternion -> Int -> Flt -> Normal
normalJulia p c mi e = {-# SCC "normalJulia" #-} normalize v where
   v = mkV (gx, gy, gz)
   (gx, gy, gz) = (qlen gx2' - qlen gx1', qlen gy2' - qlen gy1', qlen gz2' - qlen gz1')
   qp = qpromote p
   (dx, gx1, gx2) = (qpromote $ mkV (e, 0, 0), qp `qsub` dx, qp `qadd` dx)
   (dy, gy1, gy2) = (qpromote $ mkV (0, e, 0), qp `qsub` dy, qp `qadd` dy)
   (dz, gz1, gz2) = (qpromote $ mkV (0, 0, e), qp `qsub` dz, qp `qadd` dz)
   v' = {-# SCC "normalJulia.iter'" #-} iter' [gx1, gx2, gy1, gy2, gz1, gz2] c mi
   (gx1':gx2':gy1':gy2':gz1':gz2':[]) = v'
   
-- | iterates several @Quaternion@s together
iter'
   :: [Quaternion] -- ^ the quaternions to iterate
   -> Quaternion -- ^ the c value
   -> Int -- ^ the number of iterations
   -> [Quaternion]
   
iter' qs _ 0 = qs
iter' qs c n = iter' qs' c (n-1) where
   qs' = map (qadd c . qsq) qs

-- | if the magnitude of the quaternion exceeds this value it is considered
-- to diverge
escapeThreashold :: Flt
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
   { real :: {-# UNPACK #-} ! Flt
   , imag :: {-# UNPACK #-} ! Vector
   } deriving (Show, Eq)

qzero :: Quaternion
qzero = Quaternion 1 $ vpromote 0

qlen :: Quaternion -> Flt
qlen (Quaternion r (Vector i j k)) = sqrt $ r*r + i*i + j*j + k*k

qscale :: Quaternion -> Flt -> Quaternion
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

--------------------------------------------------------------------------------
-- Menger Sponge
--------------------------------------------------------------------------------

buildMenger :: Shape -> Material -> Int -> Transform -> [AnyPrim]
buildMenger s m level trans = go level identity where
   putAt t = mkAnyPrim $ mkGeom t False m Nothing s (-1)
   go 0 t = [putAt (concatTrans (scale $ mkV (2.2, 2.2, 2.2)) t `concatTrans` trans) ]
   go l t = concatMap (go (l-1)) ts where
      ts = map (\v -> foldl concatTrans t [sc, translate v]) vs
      sc = scale $ mkV (1/3, 1/3, 1/3)
      vs = map (mkV) coords
      coords = filter vm [(x, y, z) | x <- [-1..1], y <- [-1..1], z <- [-1..1]]
      vm (a, b, c) = (length $ filter (/=0) [a, b, c]) > 1 -- valid coord?
