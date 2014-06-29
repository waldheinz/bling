
module Graphics.Bling.IO.Progress (
   (<&>), progressWriter
   ) where
   
import System.FilePath
import Text.Printf

import Graphics.Bling.IO.Bitmap
import Graphics.Bling.Rendering

-- | allows to combine two progress reporters into a new one. the new
--   reporter will request continued operation iff both of the provided
--   reporters did so
(<&>) :: ProgressReporter -> ProgressReporter -> ProgressReporter
(<&>) p1 p2 prog = do
   r1 <- p1 prog
   r2 <- p2 prog
   return $ r1 && r2

-- | a progress reporter which will write every complete rendering pass to
--   a separate, numbered file
progressWriter
   :: FilePath -- ^ the basename, will get pass number and extension appended
   -> ProgressReporter
progressWriter baseName (PassDone p img spw) = do
   putStrLn $ "\nWriting " ++ fname ++ "..."
   
   writePng img $ fname <.> "png"
   writeRgbe img $ fname <.> "hdr"
   return True
   
   where
         fname = baseName ++ "-" ++ printf "%05d" p
         
progressWriter _ _ = return True



