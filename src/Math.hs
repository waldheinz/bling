
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
data Dimension
    = X -- ^ the x - axis
    | Y -- ^ the y - axis
    | Z -- ^ the z - axis
    deriving (Show, Eq)

allDimensions :: [Dimension]
allDimensions = [X, Y, Z]

type Vector = (Flt, Flt, Flt)
type Point = Vector
type Normal = Vector

data Ray = Ray {
   rayOrigin :: Point,
   rayDir :: Normal,
   rayMin :: Flt,
   rayMax :: Flt
   } deriving Show

dominant :: Vector -> Dimension
dominant (x, y, z)
   | (ax > ay) && (ax > az) = X
   | (ay > az) = Y
   | otherwise = Z
   where
      ax = abs x
      ay = abs y
      az = abs z

-- | Creates a ray that connects the two specified points.
segmentRay :: Point -> Point -> Ray
segmentRay p1 p2 = Ray p1 p1p2 epsilon (1 - epsilon) where
   p1p2 = (p2 `sub` p1)

positionAt :: Ray -> Flt -> Point
positionAt (Ray o d _ _) t = o `add` (scalMul d t)

-- | decides if a @t@ value is in the ray's bounds
onRay :: Ray -> Flt -> Bool
onRay (Ray _ _ tmin tmax) t = t >= tmin && t <= tmax

component :: Vector -> Dimension -> Flt
component (x, y, z) dim
   | dim == X = x
   | dim == Y = y
   | otherwise = z

add :: Vector -> Vector -> Vector
add (x, y, z) (a, b, c) = (x+a, y+b, z+c)

sub :: Vector -> Vector -> Vector
sub (x, y, z) (a, b, c) = (x-a, y-b, z-c)

neg :: Vector -> Vector
neg (x, y, z) = (-x, -y, -z)

sqLen :: Vector -> Flt
sqLen (x, y, z) = (x*x + y*y + z*z)

len :: Vector -> Flt
len v = sqrt (sqLen v)

scalMul :: Vector -> Flt -> Vector
scalMul (x, y, z) f = (x*f, y*f, z*f)

cross :: Vector -> Vector -> Vector
cross (ux,uy,uz) (vx,vy,vz) = (uy*vz - uz*vy, -(ux*vz - uz*vx), ux*vy - uy*vx)

dot :: Vector -> Vector -> Flt
dot (x,y,z) (a,b,c) =  x*a + y*b + z*c;

absDot :: Vector -> Vector -> Flt
absDot v1 v2 = abs $ dot v1 v2

normalize :: Vector -> Normal
normalize v
  | (sqLen v) /= 0 = scalMul v (1 / len v)
  | otherwise = (0, 1, 0)

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
      (scalMul x ((cos phi) * sinTheta)) `add`
      (scalMul y ((sin phi) * sinTheta)) `add`
      (scalMul z cosTheta)
      )
      
-- | generates a random point on the unit sphere,
-- see http://mathworld.wolfram.com/SpherePointPicking.html
randomOnSphere :: Rand2D -> Vector
randomOnSphere (u1, u2) = (s * cos omega, s * sin omega, u) where
   u = u1 * 2 - 1
   s = sqrt (1 - (u * u))
   omega = u2 * 2 * pi
   
cosineSampleHemisphere :: Rand2D -> Vector
cosineSampleHemisphere u = (x, y, sqrt (max 0 (1 - x*x - y*y))) where
   (x, y) = concentricSampleDisk u
   
concentricSampleDisk :: Rand2D -> (Flt, Flt)
concentricSampleDisk (u1, u2) = concentricSampleDisk' (sx, sy) where
   sx = u1 * 2 - 1
   sy = u2 * 2 - 1

concentricSampleDisk' :: (Flt, Flt) -> (Flt, Flt)
concentricSampleDisk' (0, 0) = (0, 0) -- handle degeneracy at the origin
concentricSampleDisk' (sx, sy) = (r * cos theta, r * sin theta) where
   theta = theta' * pi / 4.0
   (r, theta') = 
      if (sx >= -sy)
         then if (sx > sy) -- first region
            then if (sy > 0) 
               then (sx, sy / sx)
               else (sx, 8.0 + sy / sx)  
            else (sy, 2.0 - sx / sy) -- second region
         else if (sx <= sy)
            then (-sx, 4.0 - sy / (-sx)) -- third region
            else (-sy, 6.0 + sx / (-sy)) -- fourth region

sphericalDirection :: Flt -> Flt -> Flt -> Vector
sphericalDirection sint cost phi = (sint * cos phi, sint * sin phi, cost)

sphericalTheta :: Vector -> Float
sphericalTheta (_, _, z) = acos $ max (-1) $ min 1 z

sphericalPhi :: Vector -> Float
sphericalPhi (x, y, _)
   | p' < 0 = p' + 2 * pi
   | otherwise = p'
   where
         p' = atan2 y x

data LocalCoordinates = LocalCoordinates Vector Vector Vector

coordinateSystem :: Vector -> LocalCoordinates
coordinateSystem v@(x, y, z)
   | abs x > abs y = 
      let 
          invLen = 1.0 / (sqrt (x*x + z*z))
          v2 = (-z * invLen, 0, x * invLen)
      in LocalCoordinates v2 (cross v v2) v
   | otherwise = 
      let
          invLen = 1.0 / (sqrt (y*y + z*z))
          v2 = (0, z * invLen, -y * invLen)
      in LocalCoordinates v2 (cross v v2) v
      
worldToLocal :: LocalCoordinates -> Vector -> Vector
worldToLocal (LocalCoordinates sn tn nn) v = (dot v sn, dot v tn, dot v nn)

localToWorld :: LocalCoordinates -> Vector -> Vector
localToWorld (LocalCoordinates (sx, sy, sz) (tx, ty, tz) (nx, ny, nz)) (x, y, z) =
   (sx * x + tx * y + nx * z,
    sy * x + ty * y + ny * z,
    sz * x + tz * y + nz * z)

