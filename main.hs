--- RT - H

import Control.Monad
import Control.Monad.ST
import System.IO
import System.Random.MWC
import Text.Printf
import Time

import Image
import Pathtracer
import Random
import Scene
import DefaultScenes

myScene :: Scene
myScene = sphereCube (fromIntegral resX / fromIntegral resY)

resX :: Int
resX = 800

resY :: Int
resY = 600

passSamples :: Int
passSamples = 1

onePass :: Gen s -> Image s -> Int-> Scene -> Integrator -> ST s ()
onePass gen img ns scene int = do
   ox <-  runRandST gen $ rndR (0, 1 / fromIntegral ns)
   oy <-  runRandST gen $ rndR (0, 1 / fromIntegral ns)
   sequence_ $ map (apply . shift (ox, oy)) $ imageSamples ns (imageWidth img) (imageHeight img)
      where
         sx = fromIntegral $ imageWidth img
         sy = fromIntegral $ imageHeight img
         apply (px, py) = do
            ws <- runRandST gen $ int scene ((sceneCam scene) (px / sx, py / sy))
            addSample img (ImageSample px py ws)

stratify :: Int -> [(Float, Float)] -> [(Float, Float)]
stratify _ [] = []
stratify 1 xs = xs
stratify n (p:xs) = stratify' p ++ (stratify n xs) where
   stratify' (px, py) = map (shift (px, py)) [(fromIntegral x / fn, fromIntegral y / fn) | y <- [0..n-1], x <- [0..n-1]]
   fn = fromIntegral n

shift :: (Float, Float) -> (Float, Float) -> (Float, Float)
shift (ox, oy) (x, y) = (x + ox, y + oy)

imageSamples :: Int -> Int -> Int -> [(Float, Float)] -- TODO: should account for filter extent
imageSamples ns sx sy = stratify ns [ (fromIntegral x, fromIntegral y) | y <- [0..sy-1], x <- [0..sx-1]]
   
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
   
render :: Int -> (Image RealWorld) -> Scene -> Integrator -> IO ()
render pass img sc int = do
   putStrLn "Rendering..."
   start <- getClockTime
 --  seed <- randomIO :: IO Int
   gen <- stToIO $ mkRndGen pass
   stToIO $ onePass gen img passSamples sc int
   stop <- getClockTime
   putStrLn (pretty $ diffClockTimes stop start)
   
   putStrLn $ "Writing " ++ fname ++ "..."
   h1 <- openFile (fname ++ ".ppm") WriteMode
   writePpm img h1
   hClose h1

   h2 <- openFile (fname ++ ".hdr") WriteMode
   writeRgbe img h2
   hClose h2
   
   render (pass + 1) img sc int
   where
         fname = "pass-" ++ (printf "%05d" pass)

main :: IO ()
main = do
   putStrLn "Creating image..."
   img <- stToIO $  mkImage resX resY
   render 1 img myScene pathTracer
         