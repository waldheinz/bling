
module Graphics.Bling.Integrator.Metropolis (

   Metropolis, mkMLT

   ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.Path
import Graphics.Bling.Random as R
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data Metropolis = MLT
   { _integrator :: PathIntegrator
   , _ppp      :: Int -- ^ particles per pass
   }

maxDepth :: Int
maxDepth = 7

mkMLT :: Metropolis
mkMLT = MLT (mkPathIntegrator maxDepth) 10000

instance Printable Metropolis where
   prettyPrint (MLT integ ppp) = PP.vcat [
      PP.text "metropolis light transport",
      PP.text "integrator" PP.<+> prettyPrint integ,
      PP.int ppp PP.<+> PP.text "photons per pass" ]

instance Renderer Metropolis where
   render (MLT integ ppp) job report = pass img where
      scene = jobScene job
      img = mkJobImage job
      pass i = do
            seed <- ioSeed

            img' <- stToIO $ do
               mimg <- thaw i
               
               runWithSeed seed $ do
                  s <- initialSample
                  x <- liftR $ newSTRef s
                  x' <- evalSample scene integ s
                  
                  replicateM_ ppp $ do
                     s' <- liftR $ readSTRef x
                     smp' <- mutate s'
                     x'' <- evalSample scene integ smp'
                     let iProp = evalI x''
                     let iCurr = evalI x'
                     let a = min 1 (iProp / iCurr)
                     
                     R.rnd >>= \r -> if r < a
                        then liftR $ writeSTRef x (smp')
                        else return ()

                     -- record sample
                     sr <- liftR $ readSTRef x
                     sr' <- evalSample scene integ sr
                     liftR $ splatSample mimg sr'
                  
               freeze mimg

            cont <- report $ (PassDone 1 img')
            if cont
               then pass img'
               else return ()

initialSample :: Rand s (Sample s)
initialSample = do
   v1d <- liftR $ V.new n1d
   v2d <- liftR $ V.new n2d
   
   forM_ [0..n1d-1] $ \i -> do
      x <- R.rnd
      liftR $ V.write v1d i x

   forM_ [0..n2d-1] $ \i -> do
      x <- R.rnd2D
      liftR $ V.write v2d i x

   ox <- R.rnd
   oy <- R.rnd
   luv <- R.rnd2D
   let cs = CameraSample (lerp ox 0 480) (lerp oy 0 480) luv
   
   return $ mkPrecompSample cs v1d v2d
   where
      n1d = maxDepth * 3
      n2d = maxDepth * 3

mutate :: Sample s -> Rand s (Sample s)
mutate smp = initialSample

evalI :: ImageSample -> Flt
evalI (ImageSample _ _ (_, ss)) = sY ss

evalSample :: (SurfaceIntegrator i) => Scene -> i -> Sample s -> Rand s ImageSample
evalSample scn si smp = do
   smps <- liftR $ newSTRef []
   (flip randToSampled) smp $ do
      ray <- (fireRay (sceneCam scn))
      contrib si scn (\is -> modifySTRef smps (is :)) ray
   liftR $ head `liftM` readSTRef smps
   