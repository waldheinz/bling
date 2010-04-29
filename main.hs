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
import Math
import Pathtracer
import Plastic
import Primitive
import Random
import Scene
import Specular
import Texture
import Whitted()

blub :: Sphere
blub = Sphere 1.1 (-4.3,0,0)

blubLight :: Light
blubLight = AreaLight (fromXyz (1.0,1.0,1.0)) (MkAnyBound blub)

defMat :: Plastic
defMat = Plastic
   (MkAnyTexture $ GraphPaper 0.08 (fromXyz (0.7, 0.7, 0.7)) (fromXyz (0.05, 0.05, 0.05)))
   (MkAnyTexture $ Constant $ fromXyz (0.99, 0.99, 0.99))
   0.5

myShape :: Group
myShape = Group [
   gP (Sphere (1.1) (-2.3, 0.0, 0)) (Mirror $ fromXyz (0.9, 0.3, 0.3)) Nothing,
--   gP blub Blackbody (Just blubLight),
 --  gP (Sphere (0.6) (-1.3, 0, 0)) BluePaint Nothing,
--   gP (Plane (3) (0, 0, -1)) (BluePaint) Nothing,
 --  gP (Plane (5) (1, 0, 0)) defMat Nothing,
 --  gP (Plane (5) (-1, 0, 0)) defMat Nothing,
   gP (Plane (1.1) (0, 1, 0)) defMat Nothing ]

myLights :: [Light]
myLights = [
--    blubLight
    Directional (fromXyz (2, 2, 2)) (normalize (-2, 2, -2))
--      SoftBox (0.8, 0.8, 0.8)
    ]

resX :: Int
resX = 640

resY :: Int
resY = 480

myView :: View
myView = View (10, 3, -4) (0,-0.5,0) (0, 1, 0) 1.8 (fromIntegral resX / fromIntegral resY)

myCamera :: Camera
myCamera = pinHoleCamera myView

myScene :: Scene
myScene = Scene (MkAnyPrimitive myShape) myLights

onePass :: Image -> Scene -> Camera -> Integrator -> Rand Image
onePass img scene cam int = do
   ox <- rndR (0, 1 / fromIntegral ns)
   oy <- rndR (0, 1 / fromIntegral ns)
   apply img $ map (shift (ox, oy)) $ stratify ns $ imageSamples img
      where
         ns = 2
         sx = fromIntegral $ imageWidth img
         sy = fromIntegral $ imageHeight img
         apply :: Image -> [(Float, Float)] -> Rand Image
         apply i [] = return $! i
         apply i ((px, py):xs)
             | seq i False = undefined
             | otherwise = do
            ws <- int scene (cam (px / sx, py / sy))
            nxs <- seq ws return $! (ImageSample px py ws)
            apply (nxs `seq` i `seq` addSample i nxs) xs
      
stratify :: Int -> [(Float, Float)] -> [(Float, Float)]
stratify _ [] = []
stratify 1 xs = xs
stratify n (p:xs) = stratify' p ++ (stratify n xs) where
   stratify' (px, py) = map (shift (px, py)) [(fromIntegral x / fn, fromIntegral y / fn) | y <- [0..n-1], x <- [0..n-1]]
   fn = fromIntegral n
         
shift :: (Float, Float) -> (Float, Float) -> (Float, Float)
shift (ox, oy) (x, y) = (x + ox, y + oy)
   
imageSamples :: Image -> [(Float, Float)]
imageSamples img = [ (fromIntegral x, fromIntegral y) | y <- [0..sy-1], x <- [0..sx-1]] where
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
         