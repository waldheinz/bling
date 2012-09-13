
module Graphics.Bling.Renderer.LightTracer (

   LightTracer, mkLightTracer

   ) where

import Control.Monad
import Control.Monad.ST
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Image
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Scene

data LightTracer = LightT
   { _ppp      :: {-# UNPACK #-} !Int -- ^ photons per pass
   }
   
mkLightTracer
   :: Int               -- ^ photons per pass
   -> LightTracer
mkLightTracer = LightT

instance Printable LightTracer where
   prettyPrint (LightT ppp) = PP.vcat [
      PP.text "light tracer",
      PP.int ppp PP.<+> PP.text "photons per pass" ]

instance Renderer LightTracer where
   render (LightT ppp) job report = pass 1 img where
      sc = jobScene job
      img = mkJobImage job
      pass n i = do
            seed <- ioSeed

            (img', _) <- stToIO $ do
               mimg <- thaw i
               runWithSeed seed $ replicateM_ ppp $ oneRay sc (liftR . splatSample mimg)
               freeze mimg

            cont <- report $ PassDone n img' (1 / fromIntegral (n * ppp))
            when cont $ pass (n + 1) img'
      
oneRay :: Scene -> (ImageSample -> Rand m ()) -> Rand m ()
oneRay scene splat = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li', ray, nl, pdf) = sampleLightRay scene ul ulo uld
       wo = normalize $ rayDir ray
       li = sScale li' (absDot nl wo / pdf)
       
   when ((pdf > 0) && not (isBlack li)) $
      nextVertex scene (-wo) (scene `intersect` ray) li 0 splat
      
connectCam :: Scene -> (ImageSample -> Rand s ()) -> Spectrum -> Bsdf -> Vector -> Float -> Rand s ()
connectCam sc splat li bsdf wi eps
   | isBlack f || isBlack csf || cPdf == 0 = return ()
   | sc `intersects` cray = return ()
   | otherwise = 
      splat (px, py, WS (1 / (cPdf * dCam2)) (f * li))
   where
      (CameraSampleResult csf pCam px py cPdf) = sampleCam (sceneCam sc) p
      dCam = pCam - p
      we = normalize dCam
      f = evalBsdf True bsdf wi we
      dCam2 = sqLen dCam
      cray = Ray p we eps (sqrt dCam2)
      p = bsdfShadingPoint bsdf
      
nextVertex
   :: Scene
   -> Vector
   -> Maybe Intersection
   -> Spectrum
   -> Int -- ^ depth
   -> (ImageSample -> Rand m ())
   -> Rand m ()
-- nothing hit, terminate path
nextVertex _ _ Nothing _ _ _ = return ()

nextVertex sc wi (Just int) li depth splat
   | isBlack li = return ()
   | otherwise = do
      ubc <- rnd
      ubd <- rnd2D

      let
         pcont = if depth > 3 then 0.8 else 1
         bsdf = intBsdf int
         p = bsdfShadingPoint bsdf
         (BsdfSample _ spdf bf wo) = sampleAdjBsdf bsdf wi ubc ubd
         li' = sScale (li * bf) (1  / pcont)
         int' = intersect sc $ Ray p wo (intEpsilon int) infinity
         wi' = -wo
      
      connectCam sc splat li bsdf wi $ intEpsilon int

      unless (isBlack bf || spdf == 0) $
         rnd >>= \x -> unless (x > pcont) $
            nextVertex sc wi' int' li' (depth + 1) splat
                             
