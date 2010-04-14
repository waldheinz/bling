--- RT - H

import Maybe
import System.Random
import Control.Monad

---
--- a Monad providing a PRNG
---

data Rand a = Rand (StdGen -> (a, StdGen))

instance Monad Rand where
   return k = Rand (\s -> (k, s))
   Rand c1 >>= fc2 = Rand (\s0 ->  let 
                                       (r,s1) = c1 s0 
                                       Rand c2 = fc2 r in
                                       c2 s1)

runRand :: StdGen -> Rand a -> (a, StdGen)
runRand rng (Rand c) = c rng

fromRand :: (a, StdGen) -> a
fromRand (a, _) = a

rnd :: Rand Float
rnd = Rand (randomR (0, 1::Float))

rndR :: Random a => (a, a) -> Rand a
rndR range = Rand (randomR range)

---
--- basic maths stuff used everywhere
---

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
  | otherwise = (0, 1, 0)

-- Calculate the roots of the equation a * x^2 + b * x + c = 0
roots :: Float -> Float -> Float -> [Float]
roots a b c = let d = b*b - 4*a*c
	      in if (d < 0.0) then []
	         else [ 0.5 * (-b + sqrt d), 0.5 * (-b - sqrt d) ]

--- generates a random point on the unit sphere
--- see http://mathworld.wolfram.com/SpherePointPicking.html
randomOnSphere :: Rand Vector
randomOnSphere = do
   u <- rndR (-1, 1 :: Float)
   omega <- rndR (0, 2 * pi :: Float)
   return $! ((s u) * cos omega, (s u) * sin omega, u)
   where
      s = (\u -> (sqrt (1 - (u ^ 2))))

sameHemisphere :: Vector -> Vector -> Vector
sameHemisphere v1 v2
   | (dot v1 v2) > 0 = v1
   | otherwise = neg v1

reflect :: Normal -> Point -> Rand Ray
reflect n pt = do
   rndPt <- randomOnSphere
   return (pt, (sameHemisphere rndPt n))
   
---
--- colours
---
type Spectrum = Vector -- RGB for now

black :: Spectrum
black = (0, 0, 0)

white :: Spectrum
white = (1, 1, 1)

---
--- intersections
---

epsilon :: Float
epsilon = 0.001

type Normal = Vector
type Intersection = (Point, Normal, Ray)

intPos :: Intersection -> Point -- extracts the position from an Intersection
intPos (pos, _, _) = pos

intNorm :: Intersection -> Normal -- extracts the Normal from an Intersection
intNorm (_, n, _) = n

data Shape
  = Sphere Float Point --- a sphere has a radius and a position
  | Plane Float Normal -- a plane has a distance from origin and a normal
  | Group [Shape]
  
---
--- scene definition
---

--- a scene is a Shape (most probably a group) and some light sources
data Scene = Scene Shape [Light]

--- extracts the lights from a scene
sceneLights :: Scene -> [Light]
sceneLights (Scene _ lights) = lights

sceneShape :: Scene -> Shape
sceneShape (Scene s _) = s

--- extracts the closest intersection from a list of intersections
closest :: [(Float, Intersection)] -> Intersection
closest xs = snd (foldl select (head xs) (tail xs))
  where
    select (t1, i1) (t2, i2)
      | t1 < t2 = (t1, i1)
      | otherwise = (t2, i2)
      
nearest :: Ray -> Shape -> Maybe Intersection
nearest r s
  | intersections == [] = Nothing
  | otherwise = Just (closest intersections)
  where
    intersections = intersect r s
      
--- determines all intersections of a ray and a shape
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
    normalAt t = normalize (sub (hitPoint t) center)

intGroup :: Ray -> [Shape] -> [ (Float, Intersection) ]
intGroup _ [] = []
intGroup ray (shape:rest) = (intersect ray shape) ++ (intGroup ray rest)

intersects :: Ray -> Shape -> Bool
intersects _ (Group []) = False
intersects r (Group (s:xs)) = (intersects r s) || intersects r (Group xs)
intersects r (Sphere rad center) = intersectsSphere r rad center
intersects (ro, rd) (Plane d n) = ((ro `dot` n + d) / (rd `dot` n)) < 0

intersectsSphere :: Ray -> Float -> Point -> Bool
intersectsSphere (ro, rd) rad ct = (filter (> epsilon) (roots a b c)) /= []
   where
         d = ro `sub` ct
         a = sqLen rd
         b = 2 * (d `dot` rd)
         c = (sqLen d) - (rad * rad)

---
--- Lights
---

data Light
  = Directional Normal Spectrum

---
--- a camera transforms a pixel in normalized device coordinates (NDC) to a ray
---
type Camera = (Float, Float) -> Ray

--- a very simple perspective camera that stares down the z-axis
stareDownZAxis :: Camera
stareDownZAxis (px, py) = ((0, 0, posZ), normalize dir)
  where
    posZ = -4
    dir = ((px - 0.5) * 4, (0.5 - py) * 4, -posZ)


geomFac :: Normal -> Normal -> Float
geomFac n1 n2 = max 0 ((neg n1) `dot` n2)

--- samples all lights by sampling individual lights and summing up the results
sampleAllLights :: Scene -> Intersection -> Spectrum
sampleAllLights (Scene _ []) _ = black -- no light source means black
sampleAllLights s@(Scene _ lights) i  = foldl add black spectri -- sum up contributions
  where
    spectri = map (sampleLight s i) lights 

 -- samples one randomly chosen light source
sampleOneLight :: Scene -> Intersection -> Rand Spectrum
sampleOneLight scene i = do
  lightNum <-rndR (0, lightCount - 1)
  return (sampleLight scene i (lights !! lightNum))
  where
    lightCount = length lights
    lights = sceneLights scene
    
--- samples a specific light source
sampleLight :: Scene -> Intersection -> Light -> Spectrum
sampleLight scene i (Directional ld s) = sampleDirLight scene i ld s

--- samples a directional light source
sampleDirLight :: Scene -> Intersection -> Normal -> Spectrum -> Spectrum
sampleDirLight (Scene sceneShape _) (pos, sn, ray) ld s
  | visible = unCond -- TODO: first check the color, if non-black check the visibility
  | otherwise = black
  where
    unCond = scalMul s (geomFac ld sn)
    visible = not (intersects testRay sceneShape)
    testRay = (add (scalMul outDir epsilon) pos, outDir)
    outDir = neg ld
---
--- integrators
---

--- an integrator takes a ray and a shape and computes a final color
type Integrator = Ray -> Scene -> Rand Spectrum
    
--- path tracer

pathTracer :: Integrator
pathTracer r scene@(Scene shape lights) = pathTracer' scene (nearest r shape) 0

pathTracer' :: Scene -> Maybe Intersection -> Int -> Rand Spectrum
pathTracer' _ Nothing _ = return black
pathTracer' scene (Just int@(pos, n, ray@(_, rd))) depth = do
   y <- return (sampleAllLights scene int)
   recurse <- keepGoing pc
   next <- if (recurse) then pathReflect scene int (depth + 1) else return black
   return $! (add y (scalMul next (1 / pc)))
   where
      pc = pCont depth
      
pathReflect :: Scene -> Intersection -> Int -> Rand Spectrum
pathReflect scene (pos, n, (_, inDir)) depth = do
   rayOut <- reflect n pos
   yIn <- pathTracer' scene (nearest rayOut (sceneShape scene)) depth
   return (scalMul yIn (geomFac n ((\(_, d) -> neg d) rayOut)))
   
-- rolls a dice to decide if we should continue this path,
-- returning true with the specified probability
keepGoing :: Float -> Rand Bool
keepGoing 1 = return True
keepGoing pAbort = do
   x <- rnd
   return $! (x < pAbort)

-- probability for aborting at the given recursion depth
pCont :: Int -> Float
pCont d
   | d <= 3 = 1
   | otherwise = 0.5

--- whitted - style integrator
whitted :: Integrator
whitted ray s@(Scene shape lights) = do return (light ints)
  where
    ints = intersect ray shape
    light [] = black -- background is black
    light xs = light' (closest xs) lights
      where
        light' int@(_, ns, (_, rd)) lights = scalMul (sampleAllLights s int) (geomFac ns rd)


--- the debug integrator visualizes the normals of the shapes that were hit
debug :: Integrator
debug ray (Scene shape _) = do return (color ray intersections)
  where
    intersections = intersect ray shape
    color (_, dir) [] = showDir dir -- if no shape was hit show the direction of the ray
    color _ xs = showNormal (closest xs)
    showNormal (_ , n, _) =  showDir n
    showDir (dx, dy, dz) = (abs dx, abs dy, abs dz)

---
--- sampling and reconstruction
---

-- creates the normalized device coordinates from xres and yres
ndc :: (Int, Int) -> (Int, Int) -> (Float, Float)
ndc (resX, resY) (px, py) = ((fromIntegral px / fromIntegral resX), (fromIntegral py / fromIntegral resY))

-- samples in x and y
stratify :: (Int, Int) -> (Int, Int) -> Rand [(Float, Float)]
stratify res@(resX, resY) pixel@(px, py) = do
   
   return (map (pxAdd base) offsets) where
      base = ndc res pixel
      offsets = [(x / fpps , y / fpps) | 
         x <- (map fromIntegral [0..steps-1]),
         y <- (map fromIntegral [0..steps-1]) ]
      fpps = (fromIntegral steps) * (fromIntegral resX)
      pxAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
      steps = 10
   
pixelColor :: ((Float, Float) -> Rand Spectrum) -> (Int, Int) -> (Int, Int) -> Rand Spectrum
pixelColor f res pixel@(px, py) = do
   ndcs <- stratify res pixel
   y <- (mapM f ndcs)
   return (scalMul (foldl add black y) (1 / fromIntegral spp)) where
      spp = 100
      
---
--- scene definition
---

myShape :: Shape
myShape = Group [
  (Sphere 1.0 (-1.0, 0, 0.5)),
  (Sphere 1.0 ( 1.0, 0, 0.5)),
  (Plane (1) (0, 1, 0))]

myLights :: [Light]
myLights = [
  (Directional (normalize ( 1, -2,  1)) (0.7, 0.2, 0.2)),
  (Directional (normalize ( 0, -1, -1)) (0.4, 0.4, 0.4)), 
  (Directional (normalize (-1, -2,  1)) (0.2, 0.7, 0.2))]

myScene :: Scene
myScene = Scene myShape myLights

sphereOnPlane :: Scene
sphereOnPlane = Scene 
   (Group [ Sphere 1.0 (0,0.0,0) , Plane (1) (0,1,0) ])
   ([ Directional (0, -1, 0) (0.95, 0.95, 0.9) ])

clamp :: Float -> Int
clamp v = round ( (min 1 (max 0 v)) * 255 )

makePgm :: Int -> Int -> [ Spectrum ] -> String
makePgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify(xs)
  where 
    stringify [] = ""
    stringify ((r,g,b):xs) = show (clamp r) ++ " " ++
      show (clamp g) ++ " " ++ show (clamp b) ++ " " ++
      stringify xs

noise :: (Float, Float) -> Rand Spectrum
noise (x, y) = do
   r <- rndR (0, x)
   g <- rndR (0, y)
   b <- rnd
   return (r, g, b)

main :: IO ()
main = do
   prng <- newStdGen
   writeFile "test.ppm" (makePgm resX resY (fromRand (runRand prng colours)))
   
   where
         scene = myScene
         resX = 800 :: Int
         resY = 800 :: Int
         pixels = [ (x, y) | y <- [0..resX-1], x <- [0..resY-1]]
         pixelFunc = (\ndc -> pathTracer (stareDownZAxis ndc) scene)
         colours = mapM (pixelColor pixelFunc (resX, resY)) pixels
         