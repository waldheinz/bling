{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Rendering (

   -- * Rendering

   RenderJob, mkJob, mkJobImage, jobScene, imageSizeX, imageSizeY,
   Progress(..), ProgressReporter,
  
   -- * Renderers
  
   Renderer(..), AnyRenderer, mkAnyRenderer,
   SamplerRenderer, mkSamplerRenderer,
   
   ) where

import GHC.Conc (numCapabilities)
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.IORef
import qualified System.Random.MWC as MWC
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
      _currImg :: Image
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
   render = prender
   
    
   {-
   render (SR smp si) job report = render' startImg 1 >> return () where
         scene = jobScene job
         startImg = mkJobImage job
         
         render' img' p = do
            lastImg <- newIORef img'
            
            forM_ (splitWindow $ imageWindow' img') $ \w -> do
               _ <- report $ RegionStarted w
               seed <- ioSeed
               
               (i', _) <- do
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

        -}
prender :: SamplerRenderer -> RenderJob -> ProgressReporter -> IO ()
prender (SR sampler integ) job report = do
   
   let
      scene = jobScene job
      image = mkJobImage job
      flt = imgFilter image
      wnds = splitWindow $ imageWindow' image
      
      pm f = withStrategy (parBuffer numCapabilities rdeepseq) . map f
      
      eval :: (MWC.Seed, SampleWindow) -> (Image, SampleWindow)
      eval (s, w) = runST $ do
         i <- mkImageTile flt w 
         runWithSeed s $ tile scene sampler integ i w
         i' <- fst <$> freeze i
         return (i', w)
   
   currentImage <- newIORef image

   forM_ [1..] $ \pass -> do
      seeds <- repeat <$> ioSeed
      
      forM_ (pm eval $ zip seeds wnds) $ \t -> do
         _ <- report $ RegionStarted $ snd t
         cimg <- readIORef currentImage
         (cimg', _) <- stToIO $ thaw cimg >>= \mimg -> addTile mimg t >> freeze mimg
         writeIORef currentImage cimg'
         report (SamplesAdded (snd t) cimg')
         
      i <- readIORef currentImage
      report (PassDone pass i 1)
      
   return ()
   
tile :: I.SurfaceIntegrator a =>
   Scene -> Sampler -> a -> MImage s -> SampleWindow -> Rand s ()
tile scene smp si img w = do
   let comp = fireRay cam >>= I.contrib si scene (addContrib img)
   _ <- sample smp w (I.sampleCount1D si) (I.sampleCount2D si) comp
   return ()
   where
      cam = sceneCam scene
      
