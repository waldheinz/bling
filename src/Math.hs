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
   deriving (Eq, Show)

type Point = Vector

mkPoint :: Flt -> Flt -> Flt -> Point
mkPoint = MkVector

mkPoint' :: (Flt, Flt, Flt) -> Point
mkPoint' (x, y, z) = mkPoint x y z

type Normal = Vector

mkNormal :: Flt -> Flt -> Flt -> Normal
mkNormal = MkVector

data Ray = Ray {
   rayOrigin :: Point,
   rayDir :: Normal,
   rayMin :: Flt,
   rayMax :: Flt
   } deriving Show

dominant :: Vector -> Dimension
dominant (MkVector x y z)
   | (ax > ay) && (ax > az) = dimX
   | ay > az = dimY
   | otherwise = dimZ
   where
      ax = abs x
      ay = abs y
      az = abs z

mkV :: (Flt, Flt, Flt) -> Vector
mkV (x, y, z) = MkVector x y z

-- | Creates a ray that connects the two specified points.
segmentRay :: Point -> Point -> Ray
segmentRay p1 p2 = Ray p1 p1p2 epsilon (1 - epsilon) where
   p1p2 = p2 `sub` p1

positionAt :: Ray -> Flt -> Point
positionAt (Ray o d _ _) t = o `add` scalMul d t

-- | decides if a @t@ value is in the ray's bounds
onRay :: Ray -> Flt -> Bool
onRay (Ray _ _ tmin tmax) t = t >= tmin && t <= tmax

component :: Vector -> Dimension -> Flt
{-# INLINE component #-}
component !(MkVector x y z) !d
   | d == dimX = x
   | d == dimY = y
   | otherwise = z
-- component (MkVector _ y _) dimY = y
-- component (MkVector _ _ z) dimZ = z
  
add :: Vector -> Vector -> Vector
add (MkVector x y z) (MkVector a b c) = MkVector (x+a) (y+b) (z+c)

sub :: Vector -> Vector -> Vector
sub (MkVector x y z) (MkVector a b c) = MkVector (x-a) (y-b) (z-c)

neg :: Vector -> Vector
neg (MkVector x y z) = MkVector (-x) (-y) (-z)

sqLen :: Vector -> Flt
sqLen (MkVector x y z) = x*x + y*y + z*z

len :: Vector -> Flt
len v = sqrt (sqLen v)

scalMul :: Vector -> Flt -> Vector
scalMul (MkVector x y z) f = MkVector (x*f) (y*f) (z*f)

cross :: Vector -> Vector -> Vector
cross (MkVector ux uy uz) (MkVector vx vy vz) = MkVector (uy*vz - uz*vy) (-(ux*vz - uz*vx)) (ux*vy - uy*vx)

dot :: Vector -> Vector -> Flt
dot (MkVector x y z) (MkVector a b c) =  x*a + y*b + z*c;

absDot :: Vector -> Vector -> Flt
absDot v1 v2 = abs (dot v1 v2)

normalize :: Vector -> Normal
normalize v
  | sqLen v /= 0 = scalMul v (1 / len v)
  | otherwise = MkVector 0 1 0

lerp :: Flt -> Flt -> Flt -> Flt
lerp t v1 v2 = (1 - t) * v1 + t * v2

-- | Calculate the roots of the equation a * x^2 + b * x + c = 0
solveQuadric :: Flt -> Flt -> Flt -> Maybe (Flt, Flt)
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
                  
-- | a combination strategy for multiple importance sampling
type MisHeuristic = (Int, Flt) -> (Int, Flt) -> Flt

powerHeuristic :: MisHeuristic
powerHeuristic (nf, fPdf) (ng, gPdf) = (f * f) / (f * f + g * g) where
   f = fromIntegral nf * fPdf
   g = fromIntegral ng * gPdf

balanceHeuristic :: MisHeuristic
balanceHeuristic (nf, fPdf) (ng, gPdf) = (fnf * fPdf) / (fnf * fPdf + fng * gPdf) where
   fnf = fromIntegral nf
   fng = fromIntegral ng

uniformConePdf :: Flt -> Flt
uniformConePdf cosThetaMax = 1.0 / (twoPi * (1.0 - cosThetaMax))
         
uniformSampleCone :: LocalCoordinates -> Flt -> Rand2D -> Vector
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
randomOnSphere (u1, u2) = MkVector (s * cos omega) (s * sin omega) u where
   u = u1 * 2 - 1
   s = sqrt (1 - (u * u))
   omega = u2 * 2 * pi
   
cosineSampleHemisphere :: Rand2D -> Vector
cosineSampleHemisphere u = MkVector x y (sqrt (max 0 (1 - x*x - y*y))) where
   (x, y) = concentricSampleDisk u
   
concentricSampleDisk :: Rand2D -> (Flt, Flt)
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
sphericalDirection sint cost phi = MkVector (sint * cos phi) (sint * sin phi) cost

sphericalTheta :: Vector -> Float
sphericalTheta (MkVector _ _ z) = acos $ max (-1) $ min 1 z

sphericalPhi :: Vector -> Float
sphericalPhi (MkVector x y _)
   | p' < 0 = p' + 2 * pi
   | otherwise = p'
   where
         p' = atan2 y x

data LocalCoordinates = LocalCoordinates Vector Vector Vector

coordinateSystem :: Vector -> LocalCoordinates
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
worldToLocal (LocalCoordinates sn tn nn) v = MkVector (dot v sn) (dot v tn) (dot v nn)

localToWorld :: LocalCoordinates -> Vector -> Vector
localToWorld (LocalCoordinates (MkVector sx sy sz) (MkVector tx ty tz) (MkVector nx ny nz)) (MkVector x y z) =
   MkVector
      (sx * x + tx * y + nx * z)
      (sy * x + ty * y + ny * z)
      (sz * x + tz * y + nz * z)

