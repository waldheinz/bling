
--import Control.Monad
import System (getArgs)
import System.IO
import Text.Printf
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum
import Graphics.Bling.Types
import Graphics.Bling.IO.RenderJob
import Graphics.Bling.Primitive.KdTree

prog :: Image -> ProgressReporter
prog _ (PassDone p img) = do
   putStrLn $ "\nWriting " ++ fname ++ "..."

   h2 <- openFile (fname ++ ".hdr") WriteMode
   writeRgbe img h2
   hClose h2
   return False

   where
         fname = "pass-" ++ printf "%05d" p

prog _ (SamplesAdded _) = putStr "." >> hFlush stdout >> return True
prog _ _ = return True

main :: IO ()
main = do
   args <- getArgs
   let fName = head args
   j <- fmap parseJob $ readFile fName
   
   putStrLn (PP.render (PP.text "Job Stats" PP.$$ PP.nest 3 (prettyPrint j)))
   let img = mkImage (jobPixelFilter j) (imageSizeX j) (imageSizeY j)
   let integ = mkAnySurface $ KdTreeVis $ scenePrim $ jobScene j
   let renderer = mkSamplerRenderer (mkRandomSampler 1) integ
   render renderer (jobScene j) img $ prog img

--------------------------------------------------------------------------------
-- kd-tree vision integrator
--------------------------------------------------------------------------------

data KdTreeVis = KdTreeVis KdTree

instance Printable KdTreeVis where
   prettyPrint _ = PP.text "kd tree vision"
   
instance SurfaceIntegrator KdTreeVis where
--   contrib :: (PrimMonad m) => a -> Scene -> Consumer m -> Ray -> Sampled m ()
   sampleCount1D _ = 0
   sampleCount2D _ = 0
   
   contrib (KdTreeVis t) _ tell r = do
      let stats = dbgTraverse t r
      let ints = fromIntegral $ intersections stats
      li <- mkContrib (1, fromRGB (fromIntegral $ nodesTraversed stats, ints, 0))
      liftSampled $ tell $ li
