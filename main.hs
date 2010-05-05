--- RT - H

import Control.Monad
import Control.Monad.ST
import GHC.IOBase
import System.IO
import System.Random
import System.Random.MWC
import Text.Printf
import Time

import Camera
import Color
import Geometry
import Image
import Light
import Material
import Pathtracer
import Plastic
import Primitive
import Random
import Scene
import Texture

defMat :: Material
defMat = plasticMaterial
   (graphPaper 0.08 (fromXyz (0.85, 0.85, 0.85)) (fromXyz (0.05, 0.05, 0.05)))
   (constantSpectrum $ fromXyz (0.7, 0.7, 0.7))
   0.1

plTest :: Float -> (Float, Float, Float) -> Material
plTest e kd  = plasticMaterial
   (constantSpectrum $ fromXyz kd)
   (constantSpectrum $ fromXyz (0.85, 0.85, 0.85))
   e

blub :: Sphere
blub = Sphere 0.9 (0,1,0)

blubLight :: Light
blubLight = mkAreaLight (fromXyz (0.95, 0.95, 0.95)) blub

myShape :: Primitive
myShape = Group [
 --  mkGeometricPrimitive (Sphere (0.9) (1, 1, -1)) (plTest 0.001 (1, 0.56, 0)) Nothing,
 --  mkGeometricPrimitive (Sphere (0.9) (-1, 1, -1)) (plTest 0.01 (0.38, 0.05, 0.67)) Nothing,
 --  mkGeometricPrimitive (Sphere (0.9) (-1, 1, 1)) (plTest 0.1 (1, 0.96, 0)) Nothing,
 --  mkGeometricPrimitive (Sphere (0.9) (1, 1, 1)) (plTest 1 (0.04, 0.4, 0.64)) Nothing,
   mkGeometricPrimitive blub blackBodyMaterial (Just blubLight),
   mkGeometricPrimitive (Plane (-0.1) (0, 1, 0)) (plTest 0.05 (0.95, 0.56, 0.1)) Nothing
   ]

myLights :: [Light]
myLights = [
    blubLight
--    Directional (fromXyz (2, 2, 2)) (normalize (2, 2, -2))
--      SoftBox $ fromXyz (0.95, 0.95, 0.95)
    ]

resX :: Int
resX = 640

resY :: Int
resY = 480

myView :: View
myView = View (3, 3, -8) (0,0.5,0) (0, 1, 0) 1.8 (fromIntegral resX / fromIntegral resY)

myCamera :: Camera
myCamera = pinHoleCamera myView

myScene :: Scene
myScene = Scene myShape myLights

onePass :: Gen s -> Image s -> Scene -> Camera -> Integrator -> ST s ()
onePass gen img scene cam int = do
   ox <-  runRandST gen $ rndR (0, 1 / fromIntegral ns)
   oy <-  runRandST gen $ rndR (0, 1 / fromIntegral ns)
   apply $ map (shift (ox, oy)) $ stratify ns $ imageSamples (imageWidth img) (imageHeight img)
      where
         ns = 3
         sx = fromIntegral $ imageWidth img
         sy = fromIntegral $ imageHeight img
  --       apply :: STImage s -> [(Float, Float)] -> ST s (STImage s)
         apply [] = return ()
         apply ((px, py):xs) = do
            ws <- runRandST gen $ int scene (cam (px / sx, py / sy))
            nxs <- runRandST gen $ seq ws return $! (ImageSample px py ws)
            addSample img nxs
            apply xs
            --apply (nxs `seq` i `seq` addSample i nxs) xs


stratify :: Int -> [(Float, Float)] -> [(Float, Float)]
stratify _ [] = []
stratify 1 xs = xs
stratify n (p:xs) = stratify' p ++ (stratify n xs) where
   stratify' (px, py) = map (shift (px, py)) [(fromIntegral x / fn, fromIntegral y / fn) | y <- [0..n-1], x <- [0..n-1]]
   fn = fromIntegral n
         
shift :: (Float, Float) -> (Float, Float) -> (Float, Float)
shift (ox, oy) (x, y) = (x + ox, y + oy)

imageSamples :: Int -> Int -> [(Float, Float)]
imageSamples sx sy = [ (fromIntegral x, fromIntegral y) | y <- [0..sy-1], x <- [0..sx-1]] where
   
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
   
render :: Int -> (Image RealWorld) -> Scene -> Camera -> Integrator -> IO ()
render pass img sc cam int = do
   putStrLn "Rendering..."
   start <- getClockTime
   seed <- randomIO :: IO Int
   gen <- stToIO $ mkRndGen seed
   stToIO $ onePass gen img sc cam int
   stop <- getClockTime
   putStrLn (pretty $ diffClockTimes stop start)
   putStrLn $ "Writing " ++ fname ++ "..."
   handle <- openFile fname WriteMode
   writePpm img handle
   hClose handle
   render (pass + 1) img sc cam int
   where
         fname = "pass-" ++ (printf "%05d" pass) ++ ".ppm"

main :: IO ()
main = do
   putStrLn "Creating image..."
   img <- stToIO $  mkImage resX resY
   
   render 1 img myScene myCamera pathTracer
         