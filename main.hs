--- RT - H

import Maybe
import Control.Monad
import System.Random

import Geometry
import Light
import Math
import Random

---
--- intersections
---

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

---
--- integrators
---

--- an integrator takes a ray and a shape and computes a final color
-- type Integrator = (Light a) => Ray -> Shape -> [a] -> Rand Spectrum
    
--- path tracer

-- pathTracer :: Integrator
pathTracer r shape lights = pathTracer' shape lights (nearest r shape) 0

pathTracer' :: (Light a) => Shape -> [a] -> Maybe Intersection -> Int -> Rand Spectrum
pathTracer' _ _ Nothing _ = return black
pathTracer' shape lights (Just int@(pos, n, ray@(_, rd))) depth = do
   y <- sampleOneLight shape lights int
   recurse <- keepGoing pc
   next <- if (recurse) then pathReflect shape lights int (depth + 1) else return black
   return $! (add y (scalMul next (1 / pc)))
   where
      pc = pCont depth
      
pathReflect :: (Light a) => Shape -> [a] -> Intersection -> Int -> Rand Spectrum
pathReflect shape lights (pos, n, (_, inDir)) depth = do
   rayOut <- reflect n pos
   yIn <- pathTracer' shape lights (nearest rayOut shape) depth
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
-- whitted :: Integrator
whitted ray shape lights
   | ints == [] = return black
   | otherwise = sampleAllLights shape lights (closest ints) 
  where
    ints = intersect ray shape
    
--- the debug integrator visualizes the normals of the shapes that were hit
-- debug :: Integrator
debug ray shape _ = do return (color ray intersections)
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

--myLights :: [Light]
myLights = [
  (Directional (normalize ( 1, -2,  1)) (0.7, 0.2, 0.2)),
  (Directional (normalize ( 0, -1, -1)) (0.4, 0.4, 0.4)), 
  (Directional (normalize (-1, -2,  1)) (0.2, 0.7, 0.2))]

--myScene :: Scene
--myScene = (myShape myLights)

--sphereOnPlane :: Scene
--sphereOnPlane = 
--   (Group [ Sphere 1.0 (0,0.0,0) , Plane (1) (0,1,0) ])
--   ([ Directional (0, -1, 0) (0.95, 0.95, 0.9) ])

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
         --scene = myScene
         resX = 800 :: Int
         resY = 800 :: Int
         pixels = [ (x, y) | y <- [0..resX-1], x <- [0..resY-1]]
         pixelFunc = ((\ndc -> pathTracer (stareDownZAxis ndc) myShape myLights))
         colours = mapM (pixelColor pixelFunc (resX, resY)) pixels
         