{-# LANGUAGE BangPatterns #-}

module Graphics.Bling.Math (
   module  Graphics.Bling.Types,
   
   -- * Constants
   
   twoPi, invPi, invTwoPi, infinity, epsilon,
   
   -- * Basic Functions
   
   lerp, clamp, radians, solveQuadric, atan2',
   
   -- * Vectors
   
   Vector(..), mkV, vpromote, dot, cross, normalize, absDot,
   len, sqLen,
   Normal, mkNormal, Point, mkPoint,
   Dimension, allDimensions, setComponent, (.!), dominant, dimX, dimY, dimZ,
   sphericalDirection, sphericalTheta, sphericalPhi, faceForward,
   
   -- * Rays
   
   Ray(..), normalizeRay, rayAt, segmentRay, onRay,
   
   -- * Otrth. Basis
   LocalCoordinates(..), worldToLocal, localToWorld, coordinateSystem,
   coordinateSystem',
   
   ) where

import Graphics.Bling.Types

--
-- Utility functions
--

infinity :: Flt
infinity = 1 / 0

epsilon :: Flt
epsilon = 0.001

invPi :: Flt
invPi = 1 / pi

invTwoPi :: Flt
invTwoPi = 1 / (2 * pi)

twoPi :: Flt
twoPi = 2.0 * pi

-- | converts an angle from degrees to radians
radians
   :: Flt -- ^ the angle in degrees
   -> Flt -- ^ the angle in radions
radians x = (x / 180 * pi)

-- | like @atan2@, but returns positive values in [0..2pi]
atan2' :: Flt -> Flt -> Flt
atan2' y x
   | a < 0 = a + twoPi
   | otherwise = a
   where
      a = atan2 y x

-- | clamps a value so it is withing a specified range
clamp
   :: Flt -- ^ the value to clamp
   -> Flt -- ^ the lower bound
   -> Flt -- ^ the upper bound
   -> Flt
{-# INLINE clamp #-}
clamp v lo hi
   | v < lo = lo
   | v > hi = hi
   | otherwise = v

-- | Defines names for the three axii
type Dimension = Int

dimX :: Dimension
dimX = 0

dimY :: Dimension
dimY = 1

dimZ :: Dimension
dimZ = 2

allDimensions :: [Dimension]
allDimensions = [dimX, dimY, dimZ]

lerp :: Flt -> Flt -> Flt -> Flt
{-# INLINE lerp #-}
lerp t v1 v2 = (1 - t) * v1 + t * v2

-- | Calculate the roots of the equation a * x^2 + b * x + c = 0
solveQuadric :: Flt -> Flt -> Flt -> Maybe (Flt, Flt)
{-# INLINE solveQuadric #-}
solveQuadric a b c
   | discrim < 0 = Nothing
   | otherwise = Just (min t0 t1, max t0 t1)
   where
         (t0, t1) = (q / a, c / q)
         q
            | b < 0 = -0.5 * (b - rootDiscrim)
            | otherwise = -0.5 * (b + rootDiscrim)
         rootDiscrim = sqrt discrim
         discrim = b * b - 4.0 * a * c

sphericalDirection :: Flt -> Flt -> Flt -> Vector
{-# INLINE sphericalDirection #-}
sphericalDirection sint cost phi = Vector (sint * cos phi) (sint * sin phi) cost

sphericalTheta :: Vector -> Float
{-# INLINE sphericalTheta #-}
sphericalTheta (Vector _ _ z) = acos $ max (-1) $ min 1 z

sphericalPhi :: Vector -> Float
{-# INLINE sphericalPhi #-}
sphericalPhi (Vector x y _)
   | p' < 0 = p' + 2 * pi
   | otherwise = p'
   where
         p' = atan2 y x

--
-- Vectors
--

data Vector = Vector { vx, vy, vz :: {-# UNPACK #-} !Flt } deriving ( Eq )
{-# INLINE vx #-}
{-# INLINE vy #-}
{-# INLINE vz #-}

vzip :: (Flt -> Flt -> Flt) -> Vector -> Vector -> Vector
{-# INLINE vzip #-}
vzip f (Vector x1 y1 z1) (Vector x2 y2 z2) =
   Vector (f x1 x2) (f y1 y2) (f z1 z2)

vmap :: (Flt -> Flt) -> Vector -> Vector
{-# INLINE vmap #-}
vmap f (Vector x y z) = Vector (f x) (f y) (f z)

vpromote :: Flt -> Vector
{-# INLINE vpromote #-}
vpromote x = Vector x x x

instance Show Vector where
   show (Vector x y z) = "(" ++ show x ++ ", " ++ 
      show y ++ ", " ++ show z ++ ")"

instance Num Vector where
   (+) = vzip (+)
   (-) = vzip (-)
   (*) = vzip (*)
   abs = vmap abs
   signum = vmap signum
   fromInteger = vpromote . fromInteger
      
instance Fractional Vector where
  (/) = vzip (/)
  recip = vmap recip
  fromRational = vpromote . fromRational

type Point = Vector

mkPoint :: Flt -> Flt -> Flt -> Point
mkPoint = Vector

type Normal = Vector

mkNormal :: Flt -> Flt -> Flt -> Normal
mkNormal = Vector

dominant :: Vector -> Dimension
{-# INLINE dominant #-}
dominant (Vector x y z)
   | (ax > ay) && (ax > az) = dimX
   | ay > az = dimY
   | otherwise = dimZ
   where
      ax = abs x
      ay = abs y
      az = abs z

mkV :: (Flt, Flt, Flt) -> Vector
{-# INLINE mkV #-}
mkV (x, y, z) = Vector x y z

component :: Vector -> Dimension -> Flt
{-# INLINE component #-}
component !(Vector x y z) !d
   | d == dimX = x
   | d == dimY = y
   | otherwise = z

(.!) :: Vector -> Dimension -> Flt
{-# INLINE (.!) #-}
(.!) = component

setComponent :: Dimension -> Flt -> Vector -> Vector
{-# INLINE setComponent #-}
setComponent dim t (Vector x y z)
   | dim == dimX  = mkPoint t y z
   | dim == dimY  = mkPoint x t z
   | otherwise    = mkPoint x y t

sqLen :: Vector -> Flt
{-# INLINE sqLen #-}
sqLen (Vector x y z) = x*x + y*y + z*z

len :: Vector -> Flt
{-# INLINE len #-}
len v = sqrt (sqLen v)

cross :: Vector -> Vector -> Vector
{-# INLINE cross #-}
cross (Vector ux uy uz) (Vector x2 y2 z2) =
   Vector (uy*z2 - uz*y2) (-(ux*z2 - uz*x2)) (ux*y2 - uy*x2)

dot :: Vector -> Vector -> Flt
{-# INLINE dot #-}
dot (Vector x y z) (Vector a b c) =  x*a + y*b + z*c;

absDot :: Vector -> Vector -> Flt
{-# INLINE absDot #-}
absDot v1 v2 = abs (dot v1 v2)

normalize :: Vector -> Normal
{-# INLINE normalize #-}
normalize v
  | sqLen v /= 0 = v * vpromote (1 / len v)
  | otherwise = Vector 0 1 0

faceForward :: Vector -> Vector -> Vector
faceForward v v2
   | v `dot` v2 < 0 = -v
   | otherwise = v

--
-- Rays
--

data Ray = Ray {
   rayOrigin :: {-# UNPACK #-} ! Point,
   rayDir :: {-# UNPACK #-} ! Normal,
   rayMin :: {-# UNPACK #-} ! Flt,
   rayMax :: {-# UNPACK #-} ! Flt
   } deriving Show


-- | Creates a ray that connects the two specified points.
segmentRay :: Point -> Point -> Ray
{-# INLINE segmentRay #-}
segmentRay p1 p2 = Ray p1 p1p2 epsilon (1 - epsilon) where
   p1p2 = p2 - p1

rayAt :: Ray -> Flt -> Point
{-# INLINE rayAt #-}
rayAt (Ray o d _ _) t = o + (d * vpromote t)

-- | decides if a @t@ value is in the ray's bounds
onRay :: Ray -> Flt -> Bool
{-# INLINE onRay #-}
onRay (Ray _ _ tmin tmax) t = t >= tmin && t <= tmax

-- | normalizes the direction component of a @Ray@ and adjusts the
-- min/max values accordingly
normalizeRay :: Ray -> Ray
{-# INLINE normalizeRay #-}
normalizeRay (Ray ro rd rmin rmax) = Ray ro rd' rmin' rmax' where
   l = len rd
   rmin' = rmin * l
   rmax' = rmax * l
   rd' = rd * vpromote (1 / l)

-- | an orthonormal basis
data LocalCoordinates = LocalCoordinates
    {-# UNPACK #-} ! Vector
    {-# UNPACK #-} ! Vector
    {-# UNPACK #-} ! Vector

coordinateSystem :: Vector -> LocalCoordinates
{-# INLINE coordinateSystem #-}
coordinateSystem v@(Vector x y z)
   | abs x > abs y = 
      let 
          invLen = 1.0 / sqrt (x*x + z*z)
          v2 = Vector (-z * invLen) 0 (x * invLen)
      in LocalCoordinates v2 (cross v v2) v
   | otherwise = 
      let
          invLen = 1.0 / sqrt (y*y + z*z)
          v2 = Vector 0 (z * invLen) (-y * invLen)
      in LocalCoordinates v2 (cross v v2) v

coordinateSystem' :: Vector -> Vector -> LocalCoordinates
{-# INLINE coordinateSystem' #-}
coordinateSystem' w v = LocalCoordinates u v' w' where
   w' = normalize w
   u = normalize $ v `cross` w'
   v' = w' `cross` u

worldToLocal :: LocalCoordinates -> Vector -> Vector
{-# INLINE worldToLocal #-}
worldToLocal (LocalCoordinates sn tn nn) v = Vector (dot v sn) (dot v tn) (dot v nn)

localToWorld :: LocalCoordinates -> Vector -> Vector
{-# INLINE localToWorld #-}
localToWorld (LocalCoordinates (Vector sx sy sz) (Vector tx ty tz) (Vector nx ny nz)) (Vector x y z) =
   Vector
      (sx * x + tx * y + nx * z)
      (sy * x + ty * y + ny * z)
      (sz * x + tz * y + nz * z)

