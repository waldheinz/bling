--- RT - H

import Control.Monad
import System.Random

import Camera
import Geometry
import Light
import Material
import Math
import Pathtracer
import Primitive
import Random
import Scene
import Whitted

---
--- a camera transforms a pixel in normalized device coordinates (NDC) to a ray
---
   
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
      spp = 4 :: Int

blub :: Sphere
blub = Sphere 0.6 (0,0,0)

blubLight :: Light
blubLight = AreaLight (1.0,1.0,1.0) (MkAnyBound blub)

defMat :: Matte
defMat = Matte (0.8, 0.8, 0.8)

red :: Matte
red = Matte (0.8, 0.3, 0.3)

green :: Matte
green = Matte (0.3, 0.8, 0.3)

blue :: Matte
blue = Matte (0.3, 0.3, 0.8)

myShape :: Group
myShape = Group [
 --  MkAnyPrimitive sphereGrid,
   gP (Sphere (0.6) (1.3, 0, 0)) red Nothing,
   gP blub defMat (Just blubLight),
   gP (Sphere (0.6) (-1.3, 0, 0)) green Nothing,
   gP (Plane (1.0) (0, 0, -1)) defMat Nothing,
   gP (Plane (0.6) (0, 1, 0)) defMat Nothing ]

sphereGrid :: Group
sphereGrid = Group spheres where
   spheres = map (\pos -> gP (Sphere 0.4 (sub (1, 1.0, 1) pos)) BluePaint Nothing) coords
   coords = [(x, y, z) | x <- [0..2], y <- [0..2], z <- [0..2]] :: [Vector]


myLights :: [Light]
myLights = [
   blubLight
--    Directional (2, 2, 2) (normalize (-2, 2, -2))
--      SoftBox (0.8, 0.8, 0.8)
    ]

myScene :: Scene
myScene = Scene (MkAnyPrimitive myShape) myLights

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
         pixelFunc = (\px -> whitted myScene (stareDownZAxis px))
         colours = mapM (pixelColor pixelFunc (resX, resY)) pixels
         