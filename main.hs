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
-- type Integrator = Ray -> Shape -> [t] -> Rand Spectrum
    
--- path tracer

-- pathTracer :: Integrator
pathTracer r shape lights = pathTracer' shape lights (nearest r shape) 0

pathTracer' :: (Light a) => Shape -> [a] -> Maybe Intersection -> Int -> Rand Spectrum
pathTracer' _ _ Nothing _ = return black
pathTracer' shape lights (Just int) depth = do
   y <- sampleOneLight shape lights int
   recurse <- keepGoing pc
   nextY <- if (recurse) then pathReflect shape lights int (depth + 1) else return black
   return $! (add y (scalMul nextY (1 / pc)))
   where
      pc = pCont depth
      
pathReflect :: (Light a) => Shape -> [a] -> Intersection -> Int -> Rand Spectrum
pathReflect shape lights (pos, n, _) depth = do
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
debug :: Ray -> Shape -> t -> Rand Spectrum
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
stratify res@(resX, _) pixel = do
   
   return (map (pxAdd base) offsets) where
      base = ndc res pixel
      offsets = [(x / fpps , y / fpps) | 
         x <- (map fromIntegral [0::Int .. steps-1]),
         y <- (map fromIntegral [0::Int .. steps-1]) ]
      fpps = (fromIntegral steps) * (fromIntegral resX)
      pxAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
      steps = 2
   
pixelColor :: ((Float, Float) -> Rand Spectrum) -> (Int, Int) -> (Int, Int) -> Rand Spectrum
pixelColor f res pixel = do
   ndcs <- stratify res pixel
   y <- (mapM f ndcs)
   return (scalMul (foldl add black y) (1 / fromIntegral spp)) where
      spp = 3 :: Int
      
---
--- scene definition
---

myShape :: Shape
myShape = Group [
  (sphereGrid),
  (Plane (1.4) (0, 1, 0))]

sphereGrid :: Shape
sphereGrid = Group spheres where
   spheres = map (\pos -> Sphere 0.4 (sub (1, 1.0, 1) pos)) coords
   
   coords = [(x, y, z) | x <- [0..2], y <- [0..2], z <- [0..2]] :: [Vector]

--myLights :: [Light]
{-
myLights = [
  (Directional (normalize ( 1, -2,  1)) (0.7, 0.2, 0.2)),
  (Directional (normalize ( 0, -1, -1)) (0.4, 0.4, 0.4)), 
  (Directional (normalize (-1, -2,  1)) (0.2, 0.7, 0.2))]
-}
myLights :: [InfiniteArea]
myLights = [ InfiniteArea (0.8, 0.8, 0.8) ]

clamp :: Float -> Int
clamp v = round ( (min 1 (max 0 v)) * 255 )

makePgm :: Int -> Int -> [ Spectrum ] -> String
makePgm width height s = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify s
  where 
    stringify [] = ""
    stringify ((r,g,b):xs) = show (clamp r) ++ " " ++
      show (clamp g) ++ " " ++ show (clamp b) ++ " " ++
      stringify xs

main :: IO ()
main = do
   prng <- newStdGen
   writeFile "test.ppm" (makePgm resX resY (fromRand (runRand prng colours)))
   
   where
         --scene = myScene
         resX = 800 :: Int
         resY = 800 :: Int
         pixels = [ (x, y) | y <- [0..resX-1], x <- [0..resY-1]]
         pixelFunc = ((\px -> pathTracer (stareDownZAxis px) myShape myLights))
         colours = mapM (pixelColor pixelFunc (resX, resY)) pixels
         