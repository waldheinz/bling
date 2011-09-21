{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Rendering (
  
  -- * Renderers
  
  Renderer(..), AnyRenderer, mkAnyRenderer,
  SamplerRenderer, mkSamplerRenderer,
  
  -- * Progress Report
   
   Progress(..), ProgressReporter
   ) where

import Debug.Trace
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified System.Random.MWC as MWC
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
   | PassDone
      { progPassNum :: Int
      , currImg :: Image
      }

type ProgressReporter = Progress -> IO Bool

class Printable a => Renderer a where
   render :: a -> Scene -> Image -> ProgressReporter -> IO ()
   
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
   
   render (SR smp si) scene img'' report = do
      render' img'' 1 >> return ()
      where
         render' :: Image -> Int -> IO Image
         render' img' p = do
            
            seed <- MWC.withSystemRandom $ do
               s' <- MWC.save :: MWC.Gen (PrimState IO) -> IO MWC.Seed
               return s'
                  
            i <- stToIO $ do
               img <- thaw img'
               forM_ (splitWindow $ imageWindow img) $ \w -> do
               
             --  _ <- report $ RegionStarted w
                  trace (show w) $ runWithSeed seed $ tile scene smp si img w
               freeze img
               
          --     report (SamplesAdded w) >>= \cnt ->
          --        if cnt
          --           then return ()
          --           else error "cancelled"
            
            report (PassDone p i) >>= \ cnt ->
               if cnt
                  then render' i (p + 1)
                  else return i
            
tile :: I.SurfaceIntegrator a =>
   Scene -> Sampler -> a -> MImage s -> SampleWindow -> Rand s ()
tile scene smp si img w = do
   let comp = fireRay cam >>= I.contrib si scene (addSample img)
   sample smp w (I.sampleCount1D si) (I.sampleCount2D si) comp
   where
      cam = sceneCam scene
                     