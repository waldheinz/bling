
module Math where

import Random

---
--- basic maths stuff used everywhere
---

infinity :: Float
infinity = 1 / 0 :: Float

epsilon :: Float
epsilon = 0.001

invPi :: Float
invPi = 1 / pi

invTwoPi :: Float
invTwoPi = 1 / (2 * pi)

twoPi :: Float
twoPi = 2.0 * pi

type Vector = (Float, Float, Float)
type Point = Vector
type Normal = Vector

data Ray = Ray {
   rayOrigin :: Point,
   rayDir :: Normal,
   rayMin :: Float,
   rayMax :: Float
   } deriving Show

-- | Creates a ray that connects the two specified points.
segmentRay :: Point -> Point -> Ray
segmentRay p1 p2 = Ray p1 p1p2 epsilon (1 - epsilon) where
   p1p2 = (p2 `sub` p1)

positionAt :: Ray -> Float -> Point
positionAt (Ray o d _ _) t = o `add` (scalMul d t)

-- | decides if a @t@ value is in the ray's bounds
onRay :: Ray -> Float -> Bool
onRay (Ray _ _ tmin tmax) t = t >= tmin && t <= tmax

add :: Vector -> Vector -> Vector
add (x, y, z) (a, b, c) = (x+a, y+b, z+c)

sub :: Vector -> Vector -> Vector
sub (x, y, z) (a, b, c) = (x-a, y-b, z-c)

neg :: Vector -> Vector
neg (x, y, z) = (-x, -y, -z)

sqLen :: Vector -> Float
sqLen (x, y, z) = (x*x + y*y + z*z)

len :: Vector -> Float
len v = sqrt (sqLen v)

scalMul :: Vector -> Float -> Vector
scalMul (x, y, z) f = (x*f, y*f, z*f)

cross :: Vector -> Vector -> Vector
cross (ux,uy,uz) (vx,vy,vz) = (uy*vz - uz*vy, -(ux*vz - uz*vx), ux*vy - uy*vx)

dot :: Vector -> Vector -> Float
dot (x,y,z) (a,b,c) =  x*a + y*b + z*c;

absDot :: Vector -> Vector -> Float
absDot v1 v2 = abs $ dot v1 v2

normalize :: Vector -> Normal
normalize v
  | (sqLen v) /= 0 = scalMul v (1 / len v)
  | otherwise = (0, 1, 0)

-- Calculate the roots of the equation a * x^2 + b * x + c = 0
solveQuadric :: Float -> Float -> Float -> Maybe (Float, Float)
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
         
uniformConePdf :: Float -> Float
uniformConePdf cosThetaMax = 1.0 / (twoPi * (1.0 - cosThetaMax))
         
uniformSampleCone :: LocalCoordinates -> Float -> Rand Vector
uniformSampleCone (LocalCoordinates x y z) cosThetaMax = do
   cosTheta <- rndR (cosThetaMax - epsilon, 1.0 - epsilon)
   sinTheta <- return $ sqrt (1 - cosTheta * cosTheta)
   phi <- rndR (0, twoPi)
   return (
      (scalMul x ((cos phi) * sinTheta)) `add`
      (scalMul y ((sin phi) * sinTheta)) `add`
      (scalMul z cosTheta))
      
-- | generates a random point on the unit sphere,
-- see http://mathworld.wolfram.com/SpherePointPicking.html
randomOnSphere :: Rand Vector
randomOnSphere = do
   u <- rndR (-1, 1 :: Float)
   omega <- rndR (0, 2 * pi :: Float)
   return $! ((s u) * cos omega, (s u) * sin omega, u)
   where
      s = (\u -> (sqrt (1 - (u * u))))
   
cosineSampleHemisphere :: Rand Vector
cosineSampleHemisphere = do
   (x, y) <- concentricSampleDisk
   return $! (x, y, sqrt (max 0 (1 - x*x - y*y)))
   
concentricSampleDisk :: Rand (Float, Float)
concentricSampleDisk = do
   sx <- rndR (-1, 1 :: Float)
   sy <- rndR (-1, 1 :: Float)
   return $! concentricSampleDisk' (sx, sy)

concentricSampleDisk' :: (Float, Float) -> (Float, Float)
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
