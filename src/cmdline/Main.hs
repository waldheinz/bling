
module Main ( main ) where

--import           System.Environment (getArgs)
--import           System.IO
--import qualified Text.PrettyPrint as PP
--import           Text.Printf

import           Examples
--import           Graphics.Bling.IO.Bitmap
--import           Graphics.Bling.IO.RenderJob
--import           Graphics.Bling.Rendering
--import           Graphics.Bling.Types
{-
prog :: ProgressReporter
prog (PassDone p img spw) = do
  putStrLn $ "\nWriting " ++ fname ++ "..."
  writePng img $ fname ++ ".ppm"
  writeRgbe img $ fname ++ ".hdr"
  return False
   
  where
    fname = "pass-" ++ printf "%05d" p

prog (SamplesAdded _ _) = putStr "." >> hFlush stdout >> return True
prog _ = return True
-}

main :: IO ()
main = imageFilters

{-
main = do
   args <- getArgs
   let fname = head args
   parseJob fname >>= \ result ->
      case result of
           (Left e) -> putStrLn $ show e
           (Right (job, renderer)) -> do
              putStrLn (PP.render (PP.text "Job Stats" PP.$$ PP.nest 3 (prettyPrint job)))
              render renderer job $ prog
-}

{-
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

  -}
  
