
import Control.Monad
import System (getArgs)
import System.IO
import Text.Printf
import qualified Text.PrettyPrint as PP
import Time

import Graphics.Bling.Image
import Graphics.Bling.Rendering
import Graphics.Bling.IO.RenderJob

prog :: ProgressReporter
prog (Progress (PassDone p) img) = do
   putStrLn $ "\nWriting " ++ fname ++ "..."
   h1 <- openFile (fname ++ ".ppm") WriteMode
   writePpm img h1
   hClose h1
   
   h2 <- openFile (fname ++ ".hdr") WriteMode
   writeRgbe img h2
   hClose h2
   return True
   
   where
         fname = "pass-" ++ printf "%05d" p

prog (Progress (SamplesAdded _) _) = putStr "." >> hFlush stdout >> return True
prog _ = return True

main :: IO ()
main = do
   args <- getArgs
   let fName = head args
   job <- fmap parseJob $ readFile fName

   putStrLn (PP.render (PP.text "Job Stats" PP.$$ PP.nest 3 (ppJob job)))
   render job prog
   
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
