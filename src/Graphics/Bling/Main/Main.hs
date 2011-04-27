
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
   
   imgSmp <- runRandIO $ do
      ss <- samples sampler wnd
      mapM f ss
      
   stToIO $ do
      mapM_ (addSample img) imgSmp
      
   where
      f smp = runSampledRand smp $ do
         ray <- fireRay cam
         ws <- li int sc ray
         mkImageSample ws
            
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
         
