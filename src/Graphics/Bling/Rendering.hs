{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Rendering (

   -- * Rendering

   RenderJob, mkJob, mkJobImage, jobScene, imageSizeX, imageSizeY,
   Progress(..), ProgressReporter,
  
   -- * Renderers
  
   Renderer(..), AnyRenderer, mkAnyRenderer,
   SamplerRenderer, mkSamplerRenderer,
   
   ) where

import Control.Monad
import Control.Monad.ST
import Data.IORef
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Image
import qualified Graphics.Bling.Integrator as I
import Graphics.Bling.Camera
import Graphics.Bling.Random
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Types

--------------------------------------------------------------------------------
-- Render Jobs
--------------------------------------------------------------------------------

data RenderJob = MkJob {
   jobScene :: Scene,
   jobPixelFilter :: Filter,
   imageSizeX :: Int,
   imageSizeY :: Int
   }

mkJob :: Scene -> Filter -> Int -> Int -> RenderJob
mkJob = MkJob

mkJobImage :: RenderJob -> Image
mkJobImage j = mkImage (jobPixelFilter j) (imageSizeX j) (imageSizeY j)

instance Printable RenderJob where
   prettyPrint (MkJob sc f sx sy) = PP.vcat [
      PP.text "image size" PP.<+> PP.text ((show sx) ++ "x" ++ (show sy)),
      PP.text "pixel filter" PP.<+> PP.text (show f),
      PP.text "scene" PP.$$ PP.nest 3 (prettyPrint sc)
      ]

--------------------------------------------------------------------------------
-- Rendering Progress
--------------------------------------------------------------------------------

data Progress
   = Started
   | SamplesAdded {
      progRegion :: SampleWindow,
      currImg :: Image
      }
   | RegionStarted {
      progRegStart :: SampleWindow
      }
   | PassDone
      { progPassNum :: Int
      , finalImg :: Image
      , splatWeight :: Flt
      }

type ProgressReporter = Progress -> IO Bool

class Printable a => Renderer a where
   render :: a -> RenderJob -> ProgressReporter -> IO ()
   
--
-- the existential renderer
--

data AnyRenderer = forall a . Renderer a => AR a

mkAnyRenderer :: (Renderer a) => a -> AnyRenderer
mkAnyRenderer = AR

instance Printable AnyRenderer where prettyPrint (AR a) = prettyPrint a
instance Renderer AnyRenderer where render (AR a) = render a

--
-- the sampler renderer
--

data SamplerRenderer =
   SR Sampler I.AnySurfaceIntegrator

mkSamplerRenderer
   :: Sampler
   -> I.AnySurfaceIntegrator
   -> SamplerRenderer
mkSamplerRenderer = SR

instance Printable SamplerRenderer where
   prettyPrint (SR _ _) = PP.text "sampler renderer"

instance Renderer SamplerRenderer where
   
   render (SR smp si) job report = render' startImg 1 >> return () where
         scene = jobScene job
         startImg = mkJobImage job
         
         render' img' p = do
            lastImg <- newIORef img'
            
            forM_ (splitWindow $ imageWindow' img') $ \w -> do
               _ <- report $ RegionStarted w
               seed <- ioSeed
               
               i' <- do
                  img <- readIORef lastImg

                  stToIO $ do
                     mimg <- thaw img
                     runWithSeed seed $ tile scene smp si mimg w
                     freeze mimg
               
               writeIORef lastImg i'
               
               report (SamplesAdded w i') >>= \cnt ->
                  if cnt
                     then return i'
                     else error "cancelled"

            i <- readIORef lastImg
            
            report (PassDone p i 1) >>= \ cnt ->
               if cnt
                  then render' i (p + 1)
                  else return i
            
tile :: I.SurfaceIntegrator a =>
   Scene -> Sampler -> a -> MImage s -> SampleWindow -> Rand s ()
tile scene smp si img w = do
   let comp = fireRay cam >>= I.contrib si scene (addContrib img)
   sample smp w (I.sampleCount1D si) (I.sampleCount2D si) comp
   where
      cam = sceneCam scene
