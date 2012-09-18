{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Rendering (

   -- * Rendering

   RenderJob, mkJob, mkJobImage, jobScene, jobImageSize,
   Progress(..), ProgressReporter,
  
   -- * Renderers
  
   Renderer(..), AnyRenderer, mkAnyRenderer,
   SamplerRenderer, mkSamplerRenderer,
   
   ) where

import GHC.Conc (numCapabilities)
import Control.Monad
import Control.Monad.ST
import Control.Parallel.Strategies
import qualified System.Random.MWC as MWC
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Filter
import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Camera
import Graphics.Bling.Random
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Types

--------------------------------------------------------------------------------
-- Render Jobs
--------------------------------------------------------------------------------

data RenderJob = MkJob
   { jobScene        :: !Scene
   , jobPixelFilter  :: !Filter
   , jobImageSize    :: !PixelSize
   }

mkJob :: Scene -> Filter -> PixelSize -> RenderJob
mkJob = MkJob

mkJobImage :: RenderJob -> Image
mkJobImage j = mkImage (jobPixelFilter j) (jobImageSize j)

instance Printable RenderJob where
   prettyPrint (MkJob sc f sz) = PP.vcat [
      PP.text "image size" PP.<+> PP.text (show sz),
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
      , splatWeight :: Float
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
   SR Sampler SurfaceIntegrator

mkSamplerRenderer
   :: Sampler
   -> SurfaceIntegrator
   -> SamplerRenderer
mkSamplerRenderer = SR

instance Printable SamplerRenderer where
   prettyPrint (SR _ _) = PP.text "sampler renderer"

instance Renderer SamplerRenderer where
   render = prender

prender :: SamplerRenderer -> RenderJob -> ProgressReporter -> IO ()
prender (SR sampler integ) job report = do
   let image' = mkJobImage job
   image <- thaw image'
   
   let
      scene = jobScene job
--      flt = imageFilter image
      wnds = splitWindow $ sampleExtent image
      pm f = withStrategy (parBuffer (numCapabilities) rdeepseq) . map f
      
      eval :: (MWC.Seed, SampleWindow) -> ((Image, (Int, Int)), SampleWindow)
      eval (s, w) = runST $ do
         i <- mkImageTile image' w 
         runWithSeed s $ tile scene sampler integ i w
         i' <- freeze i
         return (i', w)
      
      onePass pass = do
         seeds <- sequence $ (replicate $ length wnds) ioSeed
      
         forM_ (pm eval $ zip seeds wnds) $ \(t, w) -> do
            _ <- report $ RegionStarted w
            addTile image t
            (img', _) <- freeze image
            report (SamplesAdded w img')
         
         (img', _) <- freeze image
         report (PassDone pass img' 1) >>= \cont -> do
            if cont then onePass (pass + 1) else return ()
   
   onePass 1
   
tile :: Scene -> Sampler -> SurfaceIntegrator -> MImage (ST s) -> SampleWindow -> Rand s ()
tile scene smp si img w = do
   let comp = fireRay cam >>= surfaceLi si scene
   _ <- runSample smp w (sampleCount1D si) (sampleCount2D si) comp
   return ()
   where
      cam = sceneCam scene
      
