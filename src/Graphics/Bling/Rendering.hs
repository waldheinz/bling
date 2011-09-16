{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Rendering (
  
  -- * Renderers
  
  Renderer(..), AnyRenderer, mkAnyRenderer,
  SamplerRenderer, mkSamplerRenderer,
  
  -- * Progress Report
   
   Progress(..), ProgressReporter
   ) where

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Image
import qualified Graphics.Bling.Integrator as I
import Graphics.Bling.Camera
import Graphics.Bling.Random
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Types

data Progress
   = Started
   | SamplesAdded {
      progRegion :: SampleWindow
      }
   | RegionStarted {
      progRegStart :: SampleWindow
      }
   | PassDone {
      progPassNum :: Int
      }

type ProgressReporter = Progress -> IO Bool

class Printable a => Renderer a where
   render :: a -> Scene -> Image IO -> ProgressReporter -> IO ()
   
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
   
   render (SR smp si) scene img report = do
      render' 1
      where
         cam = sceneCam scene
         render' :: Int -> IO ()
         render' p = do
            mapM_ tile ws
            cnt <- report $ PassDone p
            if cnt
               then render' (p + 1)
               else return ()
            
            where
               tile w = do
                  _ <- report $ RegionStarted w
                  runRandIO $ renderWindow w cam si scene img smp
                  -- stToIO $ mapM_ (addSample img) is
                  cnt <- report $ SamplesAdded w
                  if cnt
                     then return ()
                     else error "cancelled"
               ws = splitWindow $ imageWindow img

--         renderWindow :: SampleWindow -> IO ()

renderWindow w cam si scene img smp = do
   let comp = fireRay cam >>= I.contrib si scene (addSample img)
   sample smp w (I.sampleCount1D si) (I.sampleCount2D si) comp
   