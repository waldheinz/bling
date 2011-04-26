
import Control.Monad
import Control.Monad.ST
import System (getArgs)
import System.IO
import Text.Printf
import qualified Text.PrettyPrint as PP
import Time

import Graphics.Bling.Camera
import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Random
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.IO.RenderJob

main :: IO ()
main = do
   args <- getArgs
   let fName = head args
   ss <- readFile fName
   let job = parseJob ss
   img <- stToIO $ mkImage (jobPixelFilter job) (imageSizeX job) (imageSizeY job)
   putStrLn (PP.render (PP.text "Job Stats" PP.$$ PP.nest 3 (ppJob job)))
   render 1 img job
   {-
onePass :: (SurfaceIntegrator a) => Gen s -> Image s -> Int -> Scene -> a -> ST s ()
onePass gen img ns scene int = do
   (ox', oy') <- runRandST gen rnd2D
   let (ox, oy) = (ox' / fromIntegral ns, oy' / fromIntegral ns)
   mapM_ (apply . shift (ox, oy)) $ imageSamples ns (imageWidth img) (imageHeight img)
      where
         apply (px, py) = do
            luv <- runRandST gen rnd2D
            ws <- runRandST gen $ li int scene (
               fireRay (sceneCam scene) (CameraSample px py luv))
            addSample img (ImageSample px py ws)

stratify :: Int -> [(Float, Float)] -> [(Float, Float)]
stratify _ [] = []
stratify 1 xs = xs
stratify n (p:xs) = stratify' p ++ stratify n xs where
   stratify' (px, py) = map (shift (px, py)) [(fromIntegral x / fn, fromIntegral y / fn) | y <- [0..n-1], x <- [0..n-1]]
   fn = fromIntegral n

shift :: (Float, Float) -> (Float, Float) -> (Float, Float)
shift (ox, oy) (x, y) = (x + ox, y + oy)

imageSamples :: Int -> Int -> Int -> [(Float, Float)] -- TODO: should account for filter extent
imageSamples ns sx sy = stratify ns [ (fromIntegral x, fromIntegral y) | y <- [0..sy-1], x <- [0..sx-1]]
   -}
   
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

pass :: Image RealWorld -> Job -> IO ()
pass img job = do
   ss <- runRandIO (samples sampler wnd)
   mapM_ f ss
   
   where
      --f :: Sample -> IO ()
      f smp = do
         ss <- runSampledIO smp $ do
            ray <- fireRay cam
            ws <- li int sc ray
            mkImageSample ws
            
         stToIO (addSample img ss)
         
      sc = jobScene job
      cam = sceneCam sc
      int = jobIntegrator job
      sampler = jobSampler job
      wnd = imageWindow img
   
render :: Int -> Image RealWorld -> Job -> IO ()
render p img job = do
   putStrLn "Rendering..."

   start <- getClockTime
   pass img job
   stop <- getClockTime
   putStrLn (pretty $ diffClockTimes stop start)
   
   putStrLn $ "Writing " ++ fname ++ "..."
   h1 <- openFile (fname ++ ".ppm") WriteMode
   writePpm img h1
   hClose h1

   h2 <- openFile (fname ++ ".hdr") WriteMode
   writeRgbe img h2
   hClose h2
   
   render (p + 1) img job
   where
         fname = "pass-" ++ printf "%05d" p
         
