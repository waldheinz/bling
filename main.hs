--- RT - H

import Control.Monad
import System.Random
import Text.Printf
import Time

import Camera
import Color
import Geometry
import Image
import Light
import Material
import Pathtracer
import Primitive
import Random
import Scene
import Specular
import Whitted()

blub :: Sphere
blub = Sphere 0.6 (0,0,0)

blubLight :: Light
blubLight = AreaLight (fromXyz (1.0,1.0,1.0)) (MkAnyBound blub)

defMat :: Matte
defMat = Matte (fromXyz (0.8, 0.8, 0.8))

red :: Matte
red = Matte (fromXyz (0.8, 0.3, 0.3))

myShape :: Group
myShape = Group [
   gP (Sphere (0.6) (1.3, 0, 0)) (Glass 1.5) Nothing,
   gP blub defMat (Just blubLight),
 --  gP (Sphere (0.6) (-1.3, 0, 0)) BluePaint Nothing,
 --  gP (Plane (2) (0, 0, -1)) defMat Nothing,
 --  gP (Plane (5) (1, 0, 0)) defMat Nothing,
 --  gP (Plane (5) (-1, 0, 0)) defMat Nothing,
   gP (Plane (0.6) (0, 1, 0)) defMat Nothing ]

myLights :: [Light]
myLights = [
    blubLight
--    Directional (2, 2, 2) (normalize (-2, 2, -2))
--      SoftBox (0.8, 0.8, 0.8)
    ]

resX :: Int
resX = 400

resY :: Int
resY = 400

myView :: View
myView = View (4, 2, -4) (1,0,0) (0, 1, 0) 1.5 (fromIntegral resX / fromIntegral resY)

myCamera :: Camera
myCamera = pinHoleCamera myView

myScene :: Scene
myScene = Scene (MkAnyPrimitive myShape) myLights

onePass :: Image -> Scene -> Camera -> Integrator -> Rand Image
onePass img scene cam int = do
   ox <- rnd
   oy <- rnd
   apply img $ pixels (ox, oy)
      where
         pixels = imageSamples img
         sx = fromIntegral $ imageWidth img
         sy = fromIntegral $ imageHeight img
         apply :: Image -> [(Float, Float)] -> Rand Image
         apply i [] = return $! i
         apply i ((px, py):xs)
             | seq i False = undefined
             | otherwise = do
            ws <- int scene (cam (px / sx, py / sy))
            ns <- return $! (ImageSample px py $ seq ws ws)
            apply (ns `seq` i `seq` addSample i ns) xs

imageSamples :: Image -> (Float, Float) -> [(Float, Float)]
imageSamples img (ox, oy) = [ (fromIntegral x + ox, fromIntegral y + oy) | y <- [0..sy-1], x <- [0..sx-1]] where
   sx = imageWidth img
   sy = imageHeight img
   
-- | Pretty print the date in '1d 9h 9m 17s' format
pretty :: TimeDiff -> String
pretty td = join . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s
   
render :: Int -> Image -> Scene -> Camera -> Integrator -> IO ()
render pass img sc cam int = do
   putStrLn "Rendering..."
   start <- getClockTime
   prng <- newStdGen
   img' <- return $! fromRand $ runRand prng (onePass img sc cam int)
   stop <- getClockTime
   putStrLn (pretty $ diffClockTimes stop start)
   putStrLn $ "Writing " ++ fname ++ "..."
   writeFile fname $ imageToPpm img'
   seq img' render (pass + 1) img' sc cam int
   where
         fname = "pass-" ++ (printf "%05d" pass) ++ ".ppm"

main :: IO ()
main = render 1 (makeImage resX resY) myScene myCamera pathTracer
         