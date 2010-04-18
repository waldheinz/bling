

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

type Vector = (Float, Float, Float)
type Point = Vector
type Normal = Vector

data Ray = Ray {
   rayOrigin :: Point,
   rayDir :: Normal,
   rayMin :: Float,
   rayMax :: Float
   }

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
cross (a,b,c) (x,y,z) = (b*z + c*y, -(a*z + c*x), a*y + b*x)

dot :: Vector -> Vector -> Float
dot (x,y,z) (a,b,c) = x*a + y*b + z*c;

absDot :: Vector -> Vector -> Float
absDot v1 v2 = abs $ dot v1 v2

normalize :: Vector -> Normal
normalize v
  | (sqLen v) /= 0 = scalMul v (1 / len v)
  | otherwise = (0, 1, 0)

-- Calculate the roots of the equation a * x^2 + b * x + c = 0
roots :: Float -> Float -> Float -> [Float]
roots a b c = let d = b*b - 4*a*c
         in if (d < 0.0) then []
            else [ 0.5 * (-b + sqrt d), 0.5 * (-b - sqrt d) ]

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

coordinateSystem :: Vector -> (Vector, Vector)
coordinateSystem v@(x, y, z)
   | abs x > abs y = 
      let 
          invLen = 1.0 / (sqrt (x*x + z*z))
          v2 = (-z * invLen, 0, x * invLen)
      in (v2, cross v v2)
   | otherwise = 
      let
          invLen = 1.0 / (sqrt (y*y + z*z))
          v2 = (0, z * invLen, -y * invLen)
      in (v2, cross v v2)
      
-- | A Spectrum of colours.
type Spectrum = Vector -- RGB for now

-- | A "black" @Spectrum@ (no transmittance or emission) at all wavelengths
black :: Spectrum
black = (0, 0, 0)

-- | A "white" @Spectrum@ (full transmission at any wavelength).
white :: Spectrum
white = (1, 1, 1)

-- | Decides if a @Spectrum@ is black (within an epsilon value).
isBlack :: Spectrum -> Bool
isBlack (r, g, b) = r < epsilon && g < epsilon && b < epsilon

sScale :: Spectrum -> Spectrum -> Spectrum
sScale (a, b, c) (d, e, f) = (a*d, b*e, c*f)
