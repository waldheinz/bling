
module Graphics.Bling.Integrator.Metropolis (

   Metropolis, mkMLT

   ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Debug.Trace
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Mutable as MV
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
   , _ppp      :: Int -- ^ mutations per pass
   , _passCount :: Int
   }

maxDepth :: Int
maxDepth = 7

mkMLT :: Int -> Int -> Metropolis
mkMLT pc mpp = MLT (mkPathIntegrator maxDepth maxDepth) mpp pc

instance Printable Metropolis where
   prettyPrint (MLT integ mpp pc) = PP.vcat [
      PP.text "metropolis light transport",
      PP.text "integrator" PP.<+> prettyPrint integ,
      PP.int mpp PP.<+> PP.text "mutations per pass",
      PP.int pc PP.<+> PP.text "passes"]

instance Renderer Metropolis where
   render (MLT integ mpp pc) job report = pass img where
      scene = jobScene job
      img = mkJobImage job
      sSmp :: Flt -> ImageSample -> ImageSample
      sSmp f (ImageSample x y (w, s)) = ImageSample x y (w * f * wt, s)
      nPixels = fromIntegral $ imgW img * imgH img
      wt = nPixels / fromIntegral (mpp * pc)
      pass i = do
            seed <- ioSeed

            img' <- stToIO $ do
               mimg <- thaw i
               
               runWithSeed seed $ do
                  (b, _) <- bootstrap scene 10000 integ
                  sCurr <- trace ("b=" ++ show b) initialSample >>= newRandRef
                  lCurr <- readRandRef sCurr >>= evalSample scene integ >>= newRandRef
                  
                  replicateM_ mpp $ do
                     sProp <- readRandRef sCurr >>= mutate
                     lProp <- evalSample scene integ sProp
                     
                     let iProp = evalI lProp
                     iCurr <- evalI `liftM` (readRandRef lCurr)
                     let a = min 1 (iProp / iCurr)
                     
                     -- record samples
                     if iCurr > 0 && not (isInfinite (1 / iCurr))
                        then do
                           lc <- readRandRef lCurr
                           liftR (splatSample mimg $ sSmp ((1 - a) * b / iCurr) lc)
                        else return ()
                     
                     if iProp > 0 && not (isInfinite (1 / iProp))
                        then liftR (splatSample mimg $ sSmp (a * b / iProp) lProp)
                        else return ()
   
                     R.rnd >>= \r -> if r < a
                        then do
                           writeRandRef sCurr sProp
                           writeRandRef lCurr lProp
                        else return ()
                     
               freeze mimg

            cont <- report $ (PassDone 1 img')
            if cont
               then pass img'
               else return ()

bootstrap :: (SurfaceIntegrator i)
   => Scene
   -> Int -- ^ number of bootstrap samples
   -> i
   -> Rand s (Flt, Sample s) -- ^ (b)
bootstrap scene n integ = do
   smps <- liftR $ MV.new n
   is <- liftR $ V.new n
   
   -- take initial set of samples to compute b
   sumI <- liftM sum $ forM [0..n-1] $ \i -> do
      smp <- initialSample
      l <- evalSample scene integ smp
      liftR $ MV.write smps i smp
      liftR $ V.write is i $ evalI l
      return $ evalI l

   -- select initial sample from bootstrap samples
 --  contribOffset <- R.rnd >>= \x -> return $ x * sumI
   
   return (sumI / fromIntegral n, undefined)

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
mutate (PrecomSample cs v1d v2d) = do
   R.rnd >>= \x -> if x < 0.25
      then initialSample
      else do

         forM_ [0..V.length v1d - 1] $ \i -> do
            v <- liftR $ V.read v1d i
            v' <- jitter v 0 1
            liftR $ V.write v1d i v'

         forM_ [0..V.length v2d - 1] $ \i -> do
            (u1, u2) <- liftR $ V.read v2d i
            u1' <- jitter u1 0 1
            u2' <- jitter u2 0 1
            liftR $ V.write v2d i (u1', u2')
         
         cs' <- mutateCamaraSample cs
         return $ PrecomSample cs' v1d v2d
mutate _ = error "mutate not precomputed sample"

mutateCamaraSample :: CameraSample -> Rand s CameraSample
mutateCamaraSample (CameraSample x y (lu, lv)) = do
   x' <- jitter x 0 480
   y' <- jitter y 0 480
   lu' <- jitter lu 0 1
   lv' <- jitter lv 0 1
   return $ CameraSample x' y' (lu', lv')

jitter :: Flt -> Flt -> Flt -> Rand s Flt
jitter v vmin vmax
   | vmin == vmax = return vmin
   | otherwise = do
      u <- R.rnd2D
      return $ jit u
   where
      jit (u1, u2) 
         | u2 < 0.5 = wrapAround vmin vmax  $ v + delta
         | otherwise = wrapAround vmin vmax  $ v - delta
         where
            delta = (vmax - vmin) * b * exp (logRat * u1)
      a = 1 / 1024
      b = 1 / 64
      logRat = -log (b / a)
      wrapAround x0 x1 x
         | x >= x1 = x0 + (x - x1)
         | x < x0 = x1 - (x0 - x)
         | otherwise = x

evalI :: ImageSample -> Flt
evalI (ImageSample _ _ (_, ss)) = sY ss

evalSample :: (SurfaceIntegrator i) => Scene -> i -> Sample s -> Rand s ImageSample
evalSample scn si smp = do
   smps <- liftR $ newSTRef []
   (flip randToSampled) smp $ do
      ray <- (fireRay (sceneCam scn))
      contrib si scn (\is -> modifySTRef smps (is :)) ray
   liftR $ head `liftM` readSTRef smps
   