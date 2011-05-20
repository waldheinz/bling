
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
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
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
   
   nextVertex scene (-(rayDir ray)) (intersect scene ray) li pdf
   
nextVertex
   :: Scene
   -> Vector
   -> Maybe Intersection
   -> Spectrum
   -> Flt
   -> Rand [ImageSample]
-- nothing hit, terminate path   
nextVertex _ _ Nothing _ _ = return []

nextVertex sc wi (Just int) li pdf = do
   uc <- rnd2D
   let (CameraSample csf pCam px py cray cpdf) = sampleCam (sceneCam sc) p uc
   let wr = rayDir cray
   let f = evalBsdf bsdf wr wi
   let dCam = len (pCam - p)
   let smpHere = ImageSample px py (dCam * absDot n wr, f * li)
   
   ubc <- rnd
   ubd <- rnd2D
   let (BsdfSample smpType spdf bf wo) = sampleBsdf bsdf wi ubc ubd
   let li' = li * bf
   let int' = intersect sc $ Ray p wo epsilon infinity
   let pdf' = pdf
   
   (liftM . (:)) smpHere $! nextVertex sc wo int' li' pdf'
   
   where
      dg = intGeometry int
      bsdf = intBsdf int
      n = dgN dg
      p = dgP dg