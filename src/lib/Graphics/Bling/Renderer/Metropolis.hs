
module Graphics.Bling.Renderer.Metropolis (

   Metropolis, mkMLT

   ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.IORef
import Debug.Trace
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Mutable as MV
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.BidirPath
import Graphics.Bling.Integrator.DirectLighting
import Graphics.Bling.Montecarlo
import Graphics.Bling.Random as R
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene

data Metropolis = MLT
   { _integrator        :: ! SurfaceIntegrator
   , _maxDepth          :: ! Int
   , _mpp               :: ! Float -- ^ mutations per pixel
   , _nbootstrap        :: ! Int
   , _plarge            :: ! Float
   , _directSamples     :: ! Int -- ^ spp for direct
   }

mkMLT
   :: Int -- ^ maximum depth
   -> Float -- ^ mutations per pixel
   -> Int -- ^ number of bootstrap samples
   -> Float -- ^ plarge
   -> Int -- ^ samples per pixel for direct lighting integrator
   -> Metropolis
mkMLT md mpp nboot pl dspp = MLT integ md mpp nboot pl dspp where
   integ = if dspp > 0
              then mkNoDirectBidirIntegrator md md
              else mkBidirPathIntegrator md md

instance Printable Metropolis where
   prettyPrint (MLT _ md mpp _ _ dspp) = PP.vcat [
      PP.text "metropolis light transport",
--      PP.text "integrator" PP.<+> prettyPrint integ,
      PP.text "max. Depth" PP.<+> PP.int md,
      PP.text "direct lighting samples" PP.<+> PP.int dspp,
      PP.float mpp PP.<+> PP.text "mutations per pixel"]

instance Renderer Metropolis where
   render (MLT integ md mpp nboot plarge dspp) job report = do
      img <- if dspp > 0
                then runDirectLighting job md dspp report
                else return $ mkJobImage job
      {-# SCC "mlt.render" #-} pass img 1 1
      where
      scene = jobScene job
      
      splat :: MImage (ST s) -> Float -> (Float, Float, Spectrum) -> ST s ()
      splat i f (x, y, s) = splatSample i x y $ sScale s (f * wt)
      
      (szx, szy) = jobImageSize job
      nPixels = szx * szy
      wt = 1 / mpp
      imgSize = (fromIntegral szx, fromIntegral szy)
      nd = (sampleCount1D integ, sampleCount2D integ)
      pass i p b = {-# SCC "mlt.onePass" #-} do
            seed <- ioSeed
            (img', bNew) <- stToIO $ do
               mimg <- thaw i
               
               b'' <- runWithSeed seed $ do
                  (b', initial) <- bootstrap scene nboot integ imgSize nd
                  sCurr <- trace ("b=" ++ show b) newRandRef initial
                  lCurr <- readRandRef sCurr >>= evalSample scene integ >>= newRandRef
                  
                  replicateM_ (floor $ mpp * fromIntegral nPixels) $ do
                     sProp <- readRandRef sCurr >>= mutate imgSize nd plarge
                     lProp <- evalSample scene integ sProp
                     
                     let iProp = evalI lProp
                     iCurr <- evalI `liftM` (readRandRef lCurr)
                     let a = min 1 (iProp / iCurr)
                     
                     -- record samples
                     
                     if iCurr > 0 && not (isInfinite (1 / iCurr))
                        then do
                           lcs <- readRandRef lCurr
                           liftR $ mapM_ (\lc -> splat mimg ((1 - a) / iCurr) lc) lcs
                        else return ()
                        
                     if iProp > 0 && not (isInfinite (1 / iProp))
                        then liftR $ mapM_ (\l -> splat mimg (a / iProp) l) lProp
                        else return ()
   
                     R.rnd >>= \r -> if r < a
                        then do
                           writeRandRef sCurr sProp
                           writeRandRef lCurr lProp
                        else return ()
                  return b'
               (x, _) <- freeze mimg
               return (x, b'')

            let bnext = lerp (1 / fromIntegral p) b bNew
               
            cont <- report $ (PassDone p img' (bnext / fromIntegral p))
            if cont
               then pass img' (p+1) bnext
               else return ()
               
runDirectLighting :: RenderJob -> Int -> Int -> (Progress -> IO Bool) -> IO Image
runDirectLighting job maxDepth dspp report =
   let
      integrator = mkDirectLightingIntegrator maxDepth
      sampler = mkStratifiedSampler' dspp
      renderer = mkSamplerRenderer sampler integrator
   in do
      imgRef <- newIORef Nothing
      render renderer job $ \prog -> case prog of
                                          (PassDone _s img _) -> do
                                             writeIORef imgRef $ Just img
                                             return False
                                             
                                          x -> report x -- pass thru

      readIORef imgRef >>= \result -> case result of
           Nothing -> error "direct lighting produced no image"
           Just img -> return img
                                       

bootstrap
   :: Scene
   -> Int -- ^ number of bootstrap samples
   -> SurfaceIntegrator -- ^ integrator to use
   -> (Float, Float) -- ^ image size
   -> (Int, Int) -- ^ (n1d, n2d) sampling needs (redundant to integ)
   -> Rand s (Float, Sample s) -- ^ (b)
bootstrap scene n integ imgSize nd = {-# SCC "bootstrap" #-} do
   smps <- liftR $ MV.new n
   is <- newRandRef []
   
   -- take initial set of samples to compute b
   sumI <- liftM sum $ forM [0..n-1] $ \i -> do
      smp <- initialSample imgSize nd
      l <- evalSample scene integ smp
      if isNaN (evalI l)
         then trace "ignoring NaN bootstrap sample" $ return 0
         else do
            liftR $ MV.write smps (n - i - 1) smp
            modifyRandRef is (evalI l :)
            return $ evalI l

   -- select initial sample from bootstrap samples
   smpDist <- mkDist1D `liftM` readRandRef is
   smpOff <- (sampleDiscrete1D smpDist) `liftM` R.rnd
   smp <- liftR $ MV.read smps $ fst smpOff
  -- smp <- initialSample imgSize nd
   return (sumI / fromIntegral n, smp)

initialSample :: (Float, Float) -> (Int, Int) -> Rand s (Sample s)
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

mutate :: (Float, Float) -> (Int, Int) -> Float -> Sample s -> Rand s (Sample s)
mutate imgSize nd@(n1d, n2d) plarge (PrecomSample cs v1d v2d) = do
   R.rnd >>= \x -> if x < plarge
      then {-# SCC "mutate.largeStep" #-} initialSample imgSize nd
      else {-# SCC "mutate.smallStep" #-} do
         v1d' <- liftR $ V.new $ V.length v1d
         forM_ [0..n1d - 1] $ \i -> do
            v <- liftR $ V.read v1d i
            v' <- jitter v 0 1
            liftR $ V.write v1d' i v'

         v2d' <- liftR $ V.new $ V.length v2d
         forM_ [0..n2d - 1] $ \i -> do
            (u1, u2) <- liftR $ V.read v2d i
            u1' <- jitter u1 0 1
            u2' <- jitter u2 0 1
            liftR $ V.write v2d' i (u1', u2')
         
         cs' <- mutateCamaraSample imgSize cs
         return $ PrecomSample cs' v1d' v2d'
mutate _ _ _ _ = error "mutate not precomputed sample"

mutateCamaraSample :: (Float, Float) -> CameraSample -> Rand s CameraSample
mutateCamaraSample (sx, sy) (CameraSample x y (lu, lv)) = do
   x' <- jitter x 0 sx
   y' <- jitter y 0 sy
   lu' <- jitter lu 0 1
   lv' <- jitter lv 0 1
   return $ CameraSample x' y' (lu', lv')

jitter :: Float -> Float -> Float -> Rand s Float
{-# INLINE jitter #-}
jitter v vmin vmax
   | vmin == vmax = return vmin
   | otherwise = go `liftM` R.rnd2D
   where
      a = 1 / 1024
      b = 1 / 256 -- 64
      logRat = -log (b / a)
      go (u1, u2)
         | u2 < 0.5 = wrapAround $ v + delta
         | otherwise = wrapAround $ v - delta
         where
            delta = (vmax - vmin) * b * exp (logRat * u1)
            
      wrapAround x
         | x >= vmax = vmin + (x - vmax)
         | x < vmin = vmax - (vmin - x)
         | otherwise = x

evalI :: [(Float, Float, Spectrum)] -> Float
evalI smps = sum $ map (\(_, _, ss) -> sY ss) smps

evalSample :: Scene -> SurfaceIntegrator -> Sample s -> Rand s [(Float, Float, Spectrum)]
evalSample scn si smp = {-# SCC "evalSample" #-} do
   smps <- liftR $ newSTRef []
   (flip randToSampled) smp $ do
      li <- fireRay (sceneCam scn) >>= surfaceLi si scn
      cs <- cameraSample
      liftSampled $ modifySTRef smps ((imageX cs, imageY cs, li) :)
      
   liftR $ readSTRef smps
   
