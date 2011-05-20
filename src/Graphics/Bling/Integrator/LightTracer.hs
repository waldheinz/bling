
module Graphics.Bling.Integrator.LightTracer (

   LightTracer, mkLightTracer
   
   ) where

import Control.Monad
import Control.Monad.ST
import Debug.Trace
import Text.PrettyPrint

import Graphics.Bling.Camera
import Graphics.Bling.Image
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Rendering
-- import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum
import Graphics.Bling.Types

data LightTracer = LightT

mkLightTracer :: LightTracer
mkLightTracer = LightT

instance Printable LightTracer where
   prettyPrint LightT = text "light tracer"

instance Renderer LightTracer where
   render LightT sc img report = pass 1 where
      pass n = do
         smps <- runRandIO $ liftM concat $ replicateM 10000 $ oneRay sc
         stToIO $ mapM_ (splatSample img) smps
         
         cnt <- report $ Progress (PassDone n) img
         if cnt
            then pass (n + 1)
            else return ()
               
oneRay :: Scene -> Rand [ImageSample]
oneRay scene = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
   
   nextVertex scene (intersect scene ray) li pdf
   
nextVertex
   :: Scene
   -> Maybe Intersection
   -> Spectrum
   -> Flt
   -> Rand [ImageSample]
-- nothing hit, terminate path   
nextVertex _ Nothing _ _ = return []

nextVertex sc (Just int) li pdf = do
   uc <- rnd2D
   let (CameraSample csf px py cray cpdf) = sampleCam (sceneCam sc) p uc
   
   return $ trace (show p ++ " -> " ++ show px ++ " " ++ show py) [ImageSample px (-py) (1, white)]
   where
      dg = intGeometry int
      bsdf = intBsdf int
      n = dgN dg
      p = dgP dg