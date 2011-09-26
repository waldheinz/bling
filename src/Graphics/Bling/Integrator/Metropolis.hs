
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
import Graphics.Bling.Montecarlo
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
   , _nbootstrap :: Int
   , _plarge :: Flt
   }

maxDepth :: Int
maxDepth = 7

mkMLT
   :: Int
   -> Int
   -> Int
   -> Flt -- ^ plarge
   -> Metropolis
mkMLT pc mpp nboot pl =
   MLT (mkPathIntegrator maxDepth maxDepth) mpp pc nboot pl

instance Printable Metropolis where
   prettyPrint (MLT integ mpp pc _ _) = PP.vcat [
      PP.text "metropolis light transport",
      PP.text "integrator" PP.<+> prettyPrint integ,
      PP.int mpp PP.<+> PP.text "mutations per pass",
      PP.int pc PP.<+> PP.text "passes"]

instance Renderer Metropolis where
   render (MLT integ mpp pc nboot plarge) job report = pass img pc where
      scene = jobScene job
      img = mkJobImage job
      sSmp :: Flt -> ImageSample -> ImageSample
      sSmp f (ImageSample x y (w, s)) = ImageSample x y (w * f * wt, s)
      nPixels = fromIntegral $ imgW img * imgH img
      wt = nPixels / fromIntegral (mpp * pc)
      imgSize = (fromIntegral $ imgW img, fromIntegral $ imgH img)
      nd = (sampleCount1D integ, sampleCount2D integ)
      pass i p
         | p == 0 = return ()
         | otherwise = do
            seed <- ioSeed
            
            img' <- stToIO $ do
               mimg <- thaw i
               
               runWithSeed seed $ do
                  (b, initial) <- bootstrap scene nboot integ imgSize nd
                  sCurr <- trace ("b=" ++ show b) newRandRef initial
                  lCurr <- readRandRef sCurr >>= evalSample scene integ >>= newRandRef
                  
                  replicateM_ mpp $ do
                     sProp <- readRandRef sCurr >>= mutate imgSize nd plarge
                     lProp <- evalSample scene integ sProp
                     
                     let iProp = evalI lProp
                     iCurr <- evalI `liftM` (readRandRef lCurr)
                     let a = min 1 (iProp / iCurr)
                     
                     -- record samples
                     if iCurr > 0 && not (isInfinite (1 / iCurr))
                        then do
                           lcs <- readRandRef lCurr
                           liftR $ mapM_ (\lc -> splatSample mimg $ sSmp ((1 - a) * b / iCurr) lc) lcs
                        else return ()
                     
                     if iProp > 0 && not (isInfinite (1 / iProp))
                        then liftR $ mapM_ (\l -> splatSample mimg $ sSmp (a * b / iProp) l) lProp
                        else return ()
   
                     R.rnd >>= \r -> if r < a
                        then do
                           writeRandRef sCurr sProp
                           writeRandRef lCurr lProp
                        else return ()
                     
               freeze mimg

            cont <- report $ (PassDone (pc - p + 1) img')
            if cont
               then pass img' (p-1)
               else return ()

bootstrap :: (SurfaceIntegrator i)
   => Scene
   -> Int -- ^ number of bootstrap samples
   -> i -- ^ integrator to use
   -> (Flt, Flt) -- ^ image size
   -> (Int, Int) -- ^ (n1d, n2d) sampling needs (redundant to integ)
   -> Rand s (Flt, Sample s) -- ^ (b)
bootstrap scene n integ imgSize nd = do
 --  smps <- liftR $ MV.new n
 --  is <- newRandRef []
   
   -- take initial set of samples to compute b
   sumI <- liftM sum $ forM [0..n-1] $ \i -> do
      smp <- initialSample imgSize nd
      l <- evalSample scene integ smp
 --     liftR $ MV.write smps (n - i - 1) smp
 --     modifyRandRef is (evalI l :)
      return $ evalI l

   -- select initial sample from bootstrap samples
 --  smpDist <- mkDist1D `liftM` readRandRef is
 --  smpOff <- (sampleDiscrete smpDist) `liftM` R.rnd
 --  smp <- liftR $ MV.read smps $ fst smpOff
   smp <- initialSample imgSize nd
   return (sumI / fromIntegral n, smp)

initialSample :: (Flt, Flt) -> (Int, Int) -> Rand s (Sample s)
initialSample (sx, sy) (n1d, n2d) = do
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
   let cs = CameraSample (lerp ox 0 sx) (lerp oy 0 sy) luv
   
   return $ mkPrecompSample cs v1d v2d

mutate :: (Flt, Flt) -> (Int, Int) -> Flt -> Sample s -> Rand s (Sample s)
mutate imgSize nd@(n1d, n2d) plarge (PrecomSample cs v1d v2d) = do
   R.rnd >>= \x -> if x < plarge
      then initialSample imgSize nd
      else do

         forM_ [0..n1d - 1] $ \i -> do
            v <- liftR $ V.read v1d i
            v' <- jitter v 0 1
            liftR $ V.write v1d i v'

         forM_ [0..n2d - 1] $ \i -> do
            (u1, u2) <- liftR $ V.read v2d i
            u1' <- jitter u1 0 1
            u2' <- jitter u2 0 1
            liftR $ V.write v2d i (u1', u2')
         
         cs' <- mutateCamaraSample imgSize cs
         return $ PrecomSample cs' v1d v2d
mutate _ _ _ _ = error "mutate not precomputed sample"

mutateCamaraSample :: (Flt, Flt) -> CameraSample -> Rand s CameraSample
mutateCamaraSample (sx, sy) (CameraSample x y (lu, lv)) = do
   x' <- jitter x 0 sx
   y' <- jitter y 0 sy
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
      b = 1 / 128 -- 64
      logRat = -log (b / a)
      wrapAround x0 x1 x
         | x >= x1 = x0 + (x - x1)
         | x < x0 = x1 - (x0 - x)
         | otherwise = x

evalI :: [ImageSample] -> Flt
evalI smps = sum $ map (\(ImageSample _ _ (_, ss)) -> sY ss) smps

evalSample :: (SurfaceIntegrator i) => Scene -> i -> Sample s -> Rand s [ImageSample]
evalSample scn si smp = do
   smps <- liftR $ newSTRef []
   (flip randToSampled) smp $ do
      ray <- (fireRay (sceneCam scn))
      contrib si scn (\is -> modifySTRef smps ((snd is) :)) ray
   liftR $ readSTRef smps
   