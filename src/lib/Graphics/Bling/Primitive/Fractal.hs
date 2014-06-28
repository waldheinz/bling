
{-# LANGUAGE BangPatterns #-}

module Graphics.Bling.Primitive.Fractal (
   
   -- * Fractals
   
   Quaternion(..), mkMandelBulb, mkJuliaQuat, 
   
) where

import qualified Data.Vector as V

import Graphics.Bling.AABB
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection

--------------------------------------------------------------------------------
-- Mandelbulb
--------------------------------------------------------------------------------

mkMandelBulb
   :: Material
   -> Int      -- ^ the order, try 8 for a start
   -> Int      -- ^ iterations, try something like 10
   -> Float    -- ^ epsilon
   -> Primitive
mkMandelBulb mat order its epsilon = prim where
   prim = Primitive inter inters bounds Nothing const
   bounds = AABB (mkPoint' n n n) (mkPoint' p p p) where
      (n, p) = (-mandelBailout, mandelBailout)
   inters = {-# SCC "mandelInters" #-} mandelInters order its epsilon
   inter ray = {-# SCC "mandelInter" #-} mandelInter order its epsilon ray >>= \(d, p, n) ->
      Just $ mkIntersection d (epsilon * 2) (mkDg' p n) prim mat 
      
mandelInters :: Int -> Int -> Float -> Ray -> Bool
mandelInters !order !its !eps !r = maybe False go (intSphere 2 r) where
  rn = normalizeRay r
  go !d
    | sqLen p > mandelBailout = False
    | dist < eps              = True
    | otherwise               = go (d + dist)
    where
      p = rayAt rn d
      (dist, _) = mandelDist order its eps p
      
mandelInter :: Int -> Int -> Float -> Ray -> Maybe (Float, Point, Normal)
mandelInter !order !its !eps !r = intSphere 2 r >>= go where
  rn = normalizeRay r
  go !d
    | sqLen p > mandelBailout = Nothing
    | dist < eps              = Just (d, p, normalize g)
    | otherwise               = go (d + dist)
    where
      p = rayAt rn d
      (!dist, !g) = mandelDist order its eps p

intSphere :: Float -> Ray -> Maybe Float
intSphere r2 (Ray ro rd rmin rmax)
   | c <= 0 = Just rmin -- start inside sphere
   | otherwise = solveQuadric a b c >>= cb
   where
         cb (t0, t1) -- check with ray bounds
            | t0 > rmax || t1 < rmin = Nothing
            | otherwise = Just t0

         c = sqLen ro - r2
         a = sqLen rd
         b = 2 * (rd `dot` ro)

-- | distance estimator for the Mandelbulb fractal
mandelDist
   :: Int               -- ^ order
   -> Int               -- ^ max. number of iterations
   -> Float             -- ^ epsilon
   -> Point             -- ^ point to estimate distance from
   -> (Float, Vector)   -- ^ (distance to bulb, gradient)
mandelDist !order !mi !eps !p
   | pot == 0 = (0, mkPoint' 0 1 0)
   | otherwise = ((0.5 / exp pot) * (sinh pot) / len gradient, gradient)
   where
      pot = mandelPotential order mi p
      gradient = (mkV (
         (mandelPotential order mi $ p + mkV (eps, 0, 0)),
         (mandelPotential order mi $ p + mkV (0, eps, 0)),
         (mandelPotential order mi $ p + mkV (0, 0, eps))) - vpromote pot) * vpromote (1 / eps)

mandelPotential :: Int -> Int -> Point -> Float
mandelPotential !order !its !pos = {-# SCC "mandelPotential" #-} go (its + 1) pos where
   go !n !z
      | n == 1 = 0
      | sqLen z' > mandelBailout = log (len z') / fromIntegral (order ^ (1 + its - n))
      | otherwise = go (n - 1) z'
      where
         z' = {-# SCC "mpz'" #-} bulbPower z order + pos

mandelBailout :: Float
mandelBailout = 2.5

bulbPower :: Point -> Int -> Point
bulbPower p 8
   | k2' <= 0  = mkPoint (0, 0, 0)
   | otherwise = mkPoint (wx, wy, wz)
   where
      (x, y, z) = (vx p, vy p, vz p)
   
      (x2, y2, z2) = (x * x, y * y, z * z)
      (x4, y4, z4) = (x2 * x2, y2 * y2, z2 * z2)
      
      k3 = x2 + z2
      k2' = sqrt $ k3 * k3 * k3 * k3 * k3 * k3 * k3
      k2 = 1 / k2'
      k1 = x4 + y4 + z4 - 6 * y2 * z2 - 6 * x2 * y2 + 2 * z2 * x2
      k4 = x2 - y2 + z2
   
      wx =  64 * x * y * z * (x2 - z2) * k4 * (x4 - 6 * x2 * z2 + z4) * k1 * k2
      wy = -16 * y2 * k3 * k4 * k4 + k1 * k1
      wz =  -8 * y*k4*(x4*x4 - 28 * x4*x2*z2 + 70 * x4*z4 - 28 * x2*z2*z4 + z4*z4)*k1*k2

bulbPower !p !n = {-# SCC "bulbPowerGeneric" #-} mkPoint (
      sin wo' * sin wi',
      cos wo',
      sin wo' * cos wi') * vpromote wr'
   where
      wr = len p
      wo = acos $ (vy p / wr)
      wi = atan2 (vx p) (vz p)
      
      wr' = wr ** fn
      wo' = wo * fn
      wi' = wi * fn
      
      fn = fromIntegral n
      
--      (x, y, z) = (vx p, vy p, vz p)
--      theta = atan2 (sqrt ((x ** 2) + (y ** 2))) z
--      phi = atan $ y / x
--      r = len p
    
--------------------------------------------------------------------------------
-- Julia Fractal
--------------------------------------------------------------------------------

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
   go !d
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
   :: V.Vector Quaternion  -- ^ the quaternions to iterate
   -> Quaternion           -- ^ the c value
   -> Int                  -- ^ the number of iterations
   -> V.Vector Quaternion
iter' qs c n = iterate (V.map $ qadd c . qsq) qs !! n

-- | if the magnitude of the quaternion exceeds this value it is considered
-- to diverge
escapeThreashold :: Float
escapeThreashold = 4

iter
   :: Quaternion                 -- ^ the quaternion to iterate
   -> Quaternion                 -- ^ the @c@ value to add in each step
   -> Int                        -- ^ the maximum number of iterations
   -> (Quaternion, Quaternion)   -- ^ the result and it's derivate
iter qi c mi = go qi qzero mi where
   go !q !qp !i
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

