
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
   { _passes   :: Int -- ^ number of passes
   , _ppp      :: Int -- ^ particles per pass
   }

mkLightTracer
   :: Int -- ^ number of passes
   -> Int -- ^ photons per pass
   -> LightTracer
mkLightTracer = LightT

instance Printable LightTracer where
   prettyPrint (LightT np ppp) = vcat [
      text "light tracer",
      int np <+> text "passes",
      int ppp <+> text "photons per pass" ]

instance Renderer LightTracer where
   render (LightT np ppp) sc img report = pass np where
      pass n
         | n == 0 = return ()
         | otherwise = do
            smps <- runRandIO $ liftM concat $ replicateM ppp $ oneRay sc
            stToIO $ mapM_ ((splatSample img) . sSmp) smps
         
            cnt <- report $ Progress (PassDone (np - n + 1)) img
            if cnt
               then pass (n - 1)
               else return ()
      
      -- | scales an image sample according to total light power in scene
      --   and total number of samples
      sSmp :: ImageSample -> ImageSample
      sSmp (ImageSample x y (w, s)) = ImageSample x y ((w * p) / cnt, s)
      cnt = fromIntegral $ np * ppp
      p = 10000000 * (sY $ lightPower sc)
      
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
   let (CameraSample csf pCam px py cray cPdf) = sampleCam (sceneCam sc) p uc
   let weye = normalize $ rayDir cray
   let f = evalBsdf bsdf weye wi
   let dCam2 = sqLen (pCam - p)
   let smpHere = ImageSample px py (absDot n weye * cPdf / dCam2, f * li)
   
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
      n = normalize $ dgN dg
      p = dgP dg
