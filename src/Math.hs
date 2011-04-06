{-# LANGUAGE BangPatterns #-}

module Math where

import Random


---
--- basic maths stuff used everywhere
---

type Flt = Float

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

radians :: Flt -> Flt
radians x = (x / 180 * pi)

-- | Defines names for the three axii
type Dimension = Int

dimX :: Dimension
dimX = 1

dimY :: Dimension
dimY = 2

dimZ :: Dimension
dimZ = 3

allDimensions :: [Dimension]
allDimensions = [dimX, dimY, dimZ]

--
-- Vectors
--

data Vector
   = MkVector
      {-# UNPACK #-} !Flt
      {-# UNPACK #-} !Flt
      {-# UNPACK #-} !Flt
   deriving (Eq)

type Point = Vector

instance Show Vector where
   show (MkVector x y z) = "(" ++ show x ++ ", " ++ 
      show y ++ ", " ++ show z ++ ")"

mkPoint :: Flt -> Flt -> Flt -> Point
mkPoint = MkVector

mkPoint' :: (Flt, Flt, Flt) -> Point
mkPoint' (x, y, z) = mkPoint x y z

type Normal = Vector

mkNormal :: Flt -> Flt -> Flt -> Normal
mkNormal = MkVector

data Ray = Ray {
   rayOrigin :: {-# UNPACK #-} ! Point,
   rayDir :: {-# UNPACK #-} ! Normal,
   rayMin :: {-# UNPACK #-} ! Flt,
   rayMax :: {-# UNPACK #-} ! Flt
   } deriving Show

data DifferentialGeometry = DifferentialGeometry {
   dgP :: {-# UNPACK #-} ! Point,
   dgN :: {-# UNPACK #-} ! Normal
   } deriving (Show)

shadingCs :: DifferentialGeometry -> LocalCoordinates
{-# INLINE shadingCs #-}
shadingCs dg = coordinateSystem $ dgN dg

dominant :: Vector -> Dimension
{-# INLINE dominant #-}
dominant (MkVector x y z)
   | (ax > ay) && (ax > az) = dimX
   | ay > az = dimY
   | otherwise = dimZ
   where
      ax = abs x
      ay = abs y
      az = abs z

mkV :: (Flt, Flt, Flt) -> Vector
{-# INLINE mkV #-}
mkV (x, y, z) = MkVector x y z

-- | Creates a ray that connects the two specified points.
segmentRay :: Point -> Point -> Ray
{-# INLINE segmentRay #-}
segmentRay p1 p2 = Ray p1 p1p2 epsilon (1 - epsilon) where
   p1p2 = p2 `sub` p1

positionAt :: Ray -> Flt -> Point
{-# INLINE positionAt #-}
positionAt (Ray o d _ _) t = o `add` scalMul d t

-- | decides if a @t@ value is in the ray's bounds
onRay :: Ray -> Flt -> Bool
{-# INLINE onRay #-}
onRay (Ray _ _ tmin tmax) t = t >= tmin && t <= tmax

component :: Vector -> Dimension -> Flt
{-# INLINE component #-}
component !(MkVector x y z) !d
   | d == dimX = x
   | d == dimY = y
   | otherwise = z

component' :: Vector -> Int -> Flt
component' = component

getX :: Vector -> Flt
{-# INLINE getX #-}
getX (MkVector x _ _) = x

getY :: Vector -> Flt
{-# INLINE getY #-}
getY (MkVector _ y _) = y

getZ :: Vector -> Flt
{-# INLINE getZ #-}
getZ (MkVector _ _ z) = z

add :: Vector -> Vector -> Vector
{-# INLINE add #-}
add (MkVector x y z) (MkVector a b c) = MkVector (x+a) (y+b) (z+c)

sub :: Vector -> Vector -> Vector
{-# INLINE sub #-}
sub (MkVector x y z) (MkVector a b c) = MkVector (x-a) (y-b) (z-c)

neg :: Vector -> Vector
{-# INLINE neg #-}
neg (MkVector x y z) = MkVector (-x) (-y) (-z)

sqLen :: Vector -> Flt
{-# INLINE sqLen #-}
sqLen (MkVector x y z) = x*x + y*y + z*z

len :: Vector -> Flt
{-# INLINE len #-}
len v = sqrt (sqLen v)

scalMul :: Vector -> Flt -> Vector
{-# INLINE scalMul #-}
scalMul (MkVector x y z) f = MkVector (x*f) (y*f) (z*f)

cross :: Vector -> Vector -> Vector
{-# INLINE cross #-}
cross (MkVector ux uy uz) (MkVector vx vy vz) = MkVector (uy*vz - uz*vy) (-(ux*vz - uz*vx)) (ux*vy - uy*vx)

dot :: Vector -> Vector -> Flt
{-# INLINE dot #-}
dot (MkVector x y z) (MkVector a b c) =  x*a + y*b + z*c;

absDot :: Vector -> Vector -> Flt
{-# INLINE absDot #-}
absDot v1 v2 = abs (dot v1 v2)

normalize :: Vector -> Normal
{-# INLINE normalize #-}
normalize v
  | sqLen v /= 0 = scalMul v (1 / len v)
  | otherwise = MkVector 0 1 0

lerp :: Flt -> Flt -> Flt -> Flt
{-# INLINE lerp #-}
lerp t v1 v2 = (1 - t) * v1 + t * v2

-- | Calculate the roots of the equation a * x^2 + b * x + c = 0
solveQuadric :: Flt -> Flt -> Flt -> Maybe (Flt, Flt)
{-# INLINE solveQuadric #-}
solveQuadric a b c
   | discrim < 0.0 = Nothing
   | otherwise = Just (min t0 t1, max t0 t1)
   where
         t0 = q / a
         t1 = c / q
         q
            | b < 0 = -0.5 * (b - rootDiscrim)
            | otherwise = -0.5 * (b + rootDiscrim)
         rootDiscrim = sqrt discrim
         discrim = b * b - 4.0 * a * c
                  

uniformConePdf :: Flt -> Flt
{-# INLINE uniformConePdf #-}
uniformConePdf cosThetaMax = 1.0 / (twoPi * (1.0 - cosThetaMax))
         
uniformSampleCone :: LocalCoordinates -> Flt -> Rand2D -> Vector
{-# INLINE uniformSampleCone #-}
uniformSampleCone (LocalCoordinates x y z) cosThetaMax (u1, u2) = let
   cosTheta = lerp u1 cosThetaMax 1.0
   sinTheta = sqrt (1 - cosTheta * cosTheta)
   phi = u2 * twoPi
   in
      (
      scalMul x (cos phi * sinTheta) `add`
      scalMul y (sin phi * sinTheta) `add`
      scalMul z cosTheta
      )
      
-- | generates a random point on the unit sphere,
-- see http://mathworld.wolfram.com/SpherePointPicking.html
randomOnSphere :: Rand2D -> Vector
{-# INLINE randomOnSphere #-}
randomOnSphere (u1, u2) = MkVector (s * cos omega) (s * sin omega) u where
   u = u1 * 2 - 1
   s = sqrt (1 - (u * u))
   omega = u2 * 2 * pi
   
cosineSampleHemisphere :: Rand2D -> Vector
{-# INLINE cosineSampleHemisphere #-}
cosineSampleHemisphere u = MkVector x y (sqrt (max 0 (1 - x*x - y*y))) where
   (x, y) = concentricSampleDisk u
   
concentricSampleDisk :: Rand2D -> (Flt, Flt)
{-# INLINE concentricSampleDisk #-}
concentricSampleDisk (u1, u2) = concentricSampleDisk' (sx, sy) where
   sx = u1 * 2 - 1
   sy = u2 * 2 - 1

concentricSampleDisk' :: (Flt, Flt) -> (Flt, Flt)
concentricSampleDisk' (0, 0) = (0, 0) -- handle degeneracy at origin
concentricSampleDisk' (sx, sy) = (r * cos theta, r * sin theta) where
   theta = theta' * pi / 4.0
   (r, theta')
      | sx >= (-sy) =
         if sx > sy then
            if sy > 0 then (sx, sy / sx) else (sx, 8.0 + sy / sx)
         else
            (sy, 2.0 - sx / sy)
      | sx <= sy = (-sx, 4.0 - sy / (-sx))
      | otherwise = (-sy, 6.0 + sx / (-sy))

sphericalDirection :: Flt -> Flt -> Flt -> Vector
{-# INLINE sphericalDirection #-}
sphericalDirection sint cost phi = MkVector (sint * cos phi) (sint * sin phi) cost

sphericalTheta :: Vector -> Float
{-# INLINE sphericalTheta #-}
sphericalTheta (MkVector _ _ z) = acos $ max (-1) $ min 1 z

sphericalPhi :: Vector -> Float
{-# INLINE sphericalPhi #-}
sphericalPhi (MkVector x y _)
   | p' < 0 = p' + 2 * pi
   | otherwise = p'
   where
         p' = atan2 y x

data LocalCoordinates = LocalCoordinates
    {-# UNPACK #-} ! Vector
    {-# UNPACK #-} ! Vector
    {-# UNPACK #-} ! Vector

coordinateSystem :: Vector -> LocalCoordinates
{-# INLINE coordinateSystem #-}
coordinateSystem v@(MkVector x y z)
   | abs x > abs y = 
      let 
          invLen = 1.0 / sqrt (x*x + z*z)
          v2 = MkVector (-z * invLen) 0 (x * invLen)
      in LocalCoordinates v2 (cross v v2) v
   | otherwise = 
      let
          invLen = 1.0 / sqrt (y*y + z*z)
          v2 = MkVector 0 (z * invLen) (-y * invLen)
      in LocalCoordinates v2 (cross v v2) v

worldToLocal :: LocalCoordinates -> Vector -> Vector
{-# INLINE worldToLocal #-}
worldToLocal (LocalCoordinates sn tn nn) v = MkVector (dot v sn) (dot v tn) (dot v nn)

localToWorld :: LocalCoordinates -> Vector -> Vector
{-# INLINE localToWorld #-}
localToWorld (LocalCoordinates (MkVector sx sy sz) (MkVector tx ty tz) (MkVector nx ny nz)) (MkVector x y z) =
   MkVector
      (sx * x + tx * y + nx * z)
      (sy * x + ty * y + ny * z)
      (sz * x + tz * y + nz * z)

