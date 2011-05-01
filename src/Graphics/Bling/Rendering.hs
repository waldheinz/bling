
module Graphics.Bling.Rendering (
   Progress(..), ProgressType(..), ProgressReporter, render
   ) where

import Control.Monad.ST
import System.Random.MWC

import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Camera
import Graphics.Bling.Random
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.IO.RenderJob

data Progress = Progress
   { progType :: ProgressType
   , progImage :: Image RealWorld
   }

data ProgressType
   = Started
   | SamplesAdded {
      progRegion :: SampleWindow
      }
   | PassDone {
      progPassNum :: Int
      }

type ProgressReporter = Progress -> IO Bool

render :: Job -> ProgressReporter -> IO ()
render j report = do
   img <- stToIO $ mkImage (jobPixelFilter j) (imageSizeX j) (imageSizeY j)
   render' 0 img
   where
         render' :: Int -> Image RealWorld -> IO ()
         render' p img = do
            mapM_ tile ws
            cnt <- report $ Progress (PassDone p) img
            case cnt of
                 True -> render' (p + 1) img
                 False -> return ()
            
            where
               tile w = do
                  is <- renderWindow j w
                  stToIO $ mapM_ (addSample img) is
                  cnt <- report $ Progress (SamplesAdded w) img
                  case cnt of
                       True -> return ()
                       False -> error "cancelled"
               ws = splitWindow $ imageWindow img
            
renderWindow :: Job -> SampleWindow -> IO [ImageSample]
renderWindow j w = withSystemRandom $ runRandST $ do
   ss <- samples sampler w
   mapM (randToSampled (fireRay cam >>= li int sc >>= mkImageSample)) ss where
      sampler = jobSampler j
      sc = jobScene j
      cam = sceneCam sc
      int = jobIntegrator j
