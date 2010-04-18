--- RT - H

import Maybe(isNothing, fromJust)
import Control.Monad
import System.Random

import Geometry
import Light
import Math
import Pathtracer
import Random
import Whitted

---
--- a camera transforms a pixel in normalized device coordinates (NDC) to a ray
---
type Camera = (Float, Float) -> Ray

--- a very simple perspective camera that stares down the z-axis
stareDownZAxis :: Camera
stareDownZAxis (px, py) = (Ray (0, 0, posZ) (normalize dir) 0 infinity)
  where
    posZ = -4
    dir = ((px - 0.5) * 4, (0.5 - py) * 4, -posZ)

---
--- integrators
---

--- whitted - style integrator
-- whitted :: Integrator
--whitted ray shape lights
--   | isNothing mint = return black
--   | otherwise = sampleAllLights shape lights (fromJust mint)
--      where
--            mint = intersect ray shape

--- the debug integrator visualizes the normals of the shapes that were hit
-- debug :: Integrator
--debug :: (Intersectable i) => Ray -> i -> t -> Rand Spectrum
--debug ray shape _
--   | isNothing mint = return black
--   | otherwise = return (showIntersection (fromJust mint))
--   where
--         mint = intersect ray shape
--         showIntersection (Intersection _ _ n) = showDir n
--         showDir (dx, dy, dz) = (abs dx, abs dy, abs dz)
         
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
      steps = 10
   
pixelColor :: ((Float, Float) -> Rand Spectrum) -> (Int, Int) -> (Int, Int) -> Rand Spectrum
pixelColor f res pixel = do
   ndcs <- stratify res pixel
   y <- (mapM f ndcs)
   return (scalMul (foldl add black y) (1 / fromIntegral spp)) where
      spp = 100 :: Int
      
---
--- scene definition
---

-- myShape :: Group
myShape = Group [
  -- MkAnyIntersectable sphereGrid, 
  MkAnyIntersectable (Sphere (1.0) (0, -0.2, 0)),
  
   MkAnyIntersectable (Plane (1.4) (0, 1, 0))]

-- sphereGrid :: Group
sphereGrid = Group spheres where
   spheres = map (\pos -> MkAnyIntersectable (Sphere 0.4 (sub (1, 1.0, 1) pos))) coords
   coords = [(x, y, z) | x <- [0..2], y <- [0..2], z <- [0..2]] :: [Vector]

--myLights :: [Light]
{-
myLights = [
  (Directional (normalize ( 1, 1, -1)) (0.9, 0.5, 0.5)),
  (Directional (normalize ( 0, 1, -1)) (0.5, 0.9, 0.5)), 
  (Directional (normalize (-1, 1, -1)) (0.5, 0.5, 0.9))]
-}
--myLights :: [SoftBox]
myLights = [ SoftBox (0.99, 0.99, 0.99) ]

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
         pixelFunc = ((\px -> whitted (stareDownZAxis px) myShape myLights))
         colours = mapM (pixelColor pixelFunc (resX, resY)) pixels
         