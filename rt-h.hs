--- RT - H

--- basic maths stuff used everywhere

type Flt = Float
type Vector = (Float, Float, Float)
type Point = Vector
type Ray = (Point, Vector) --- origin and direction

positionAt :: Ray -> Float -> Point
positionAt (origin, dir) t = origin `add` (scalMul dir t)

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
  | otherwise = (0, 0, 0)

-- Calculate the roots of the equation a * x^2 + b * x + c = 0
roots :: Float -> Float -> Float -> [Float]
roots a b c = let d = b*b - 4*a*c
	      in if (d < 0.0) then []
	         else [ 0.5 * (-b + sqrt d), 0.5 * (-b - sqrt d) ]

--- colours

type Spectrum = (Float, Float, Float) -- RGB for now

black :: Spectrum
black = (0, 0, 0)

white :: Spectrum
white = (1, 1, 1)

---
--- intersections
---

epsilon :: Float
epsilon = 0.0001

type Normal = Vector
type Intersection = (Point, Normal, Ray)

data Shape
  = Sphere Float Point --- a sphere has a radius and a position
  | Plane Float Normal -- a plane has a distance from origin and a normal
  | Group [Shape]
  
--- extracts the closest intersection from a list of intersections
closest :: [(Float, Intersection)] -> Intersection
closest xs = snd (foldl select (head xs) (tail xs))
  where
    select (t1, i1) (t2, i2)
      | t1 < t2 = (t1, i1)
      | otherwise = (t2, i2)

intersect :: Ray -> Shape -> [ (Float, Intersection) ]
intersect ray (Sphere r c) = intSphere ray r c
intersect ray (Group shapes) = intGroup ray shapes
intersect ray (Plane d n) = intPlane ray d n

intPlane :: Ray -> Float -> Normal -> [ (Float, Intersection) ]
intPlane ray@(ro, rd) d n
  | t < epsilon = []
  | otherwise = [ (t, (positionAt ray t, n, ray)) ]
  where
    t = -(ro `dot` n + d) / (rd `dot` n)

intSphere :: Ray -> Float -> Point -> [ (Float, Intersection) ]
intSphere ray@(origin, rayDir) r center = map (\t -> (t, intAt t)) times
  where
    dir = origin `sub` center
    a = sqLen rayDir
    b = 2 * (rayDir `dot` dir)
    c = (sqLen dir) - (r * r)
    times = filter (> epsilon) (roots a b c)
    hitPoint = positionAt ray
    intAt t = (hitPoint t, normalAt t, ray)
    normalAt t = normalize ((hitPoint t) `sub` center)

intGroup :: Ray -> [Shape] -> [ (Float, Intersection) ]
intGroup _ [] = []
intGroup ray (shape:rest) = (intersect ray shape) ++ (intGroup ray rest)

data Light
  = Directional Vector Spectrum
  
-- file input / output

makePgm :: Int -> Int -> [ Spectrum ] -> String
makePgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
		  where stringify [] = ""
			stringify ((r,g,b):xs) = show (round (r*255)) ++ " " 
						 ++ show (round (g*255)) ++ " " 
						 ++ show (round (b*255)) ++ " " 
						 ++ stringify xs


---
--- a camera transforms a pixel in normalized device coordinates (NDC) to a ray
---
type Camera = (Float, Float) -> Ray

--- a very simple perspective camera that stares down the z-axis
stareDownZAxis :: Camera
stareDownZAxis (px, py) = ((0, 0, posZ), normalize dir)
  where
    posZ = -4
    dir = ((px - 0.5) * 4, (py - 0.5) * 4, -posZ)

---
--- an integrator takes a ray, a shape and a number of light sources and computes a final color
---
type Integrator = Ray -> Shape -> [Light] -> Spectrum

--- the debug integrator visualizes the normals of the shapes that were hit
debug :: Integrator
debug ray shape _ = color ray intersections
  where
    intersections = intersect ray shape
    color (_, dir) [] = showDir dir -- if no shape was hit show the direction of the ray
    color _ xs = showNormal (closest xs)
    showNormal (_, normal, _) =  showDir normal
    showDir (dx, dy, dz) = (abs dx, abs dy, abs dz)
  
-- creates the normalized device coordinates from xres and yres
ndcs :: Int -> Int -> [ (Float, Float) ]
ndcs resX resY =
  let pixels = [ (x, y) | y <- [0..resY-1], x <- [0..resX-1] ]
      fResX = fromIntegral resX
      fResY = fromIntegral resY
      scale (x, y) = ((fromIntegral x) / fResX, (fromIntegral y) / fResY)
  in map scale pixels

myScene :: Shape
myScene = Group [
  (Sphere 1.00 (-0.5, 0, 0.5)),
  (Sphere 0.75 ( 0.5, 0,   0)),
  (Plane (-1) (0, 1, 0))]

makeImage :: Int -> Int -> String
makeImage resX resY =
  let fResX = fromIntegral resX
      fResY = fromIntegral resY
      pixels = ndcs resX resY
      rays = map stareDownZAxis pixels
      trace ray = (debug ray myScene [])
      colours = map trace rays
  in makePgm resX resY colours

main = do writeFile "test.ppm" (makeImage 800 800)
