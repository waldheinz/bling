module Math where

import Random

---
--- basic maths stuff used everywhere
---

infinity :: Float
infinity = 1 / 0 :: Float

epsilon :: Float
epsilon = 0.001

type Vector = (Float, Float, Float)
type Point = Vector

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
   
sameHemisphere :: Vector -> Vector -> Vector
sameHemisphere v1 v2
   | (dot v1 v2) > 0 = v1
   | otherwise = neg v1

reflect :: Normal -> Point -> Rand Ray
reflect n pt = do
   rndPt <- randomOnSphere
   return (Ray pt (sameHemisphere rndPt n) epsilon infinity)
   
type Normal = Vector
