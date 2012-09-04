
module Graphics.Bling.Integrator.LightTracer (

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
   { _passes   :: Int -- ^ number of passes
   , _ppp      :: Int -- ^ particles per pass
   }

mkLightTracer
   :: Int -- ^ number of passes
   -> Int -- ^ photons per pass
   -> LightTracer
mkLightTracer = LightT

instance Printable LightTracer where
   prettyPrint (LightT np ppp) = PP.vcat [
      PP.text "light tracer",
      PP.int np PP.<+> PP.text "passes",
      PP.int ppp PP.<+> PP.text "photons per pass" ]

instance Renderer LightTracer where
   render (LightT np ppp) job report = pass np img where
      sc = jobScene job
      img = mkJobImage job
      pass n i
         | n == 0 = return ()
         | otherwise = do
            seed <- ioSeed

            (img', _) <- stToIO $ do
               mimg <- thaw i
               runWithSeed seed $ replicateM_ ppp $ oneRay sc (liftR . (splatSample mimg) . sSmp)
               freeze mimg

            cont <- report $ PassDone (np - n + 1) img' (1 / fromIntegral (n+1))
            if cont
               then pass (n - 1) img'
               else return ()

      -- | scales an image sample according to total light power in scene
      --   and total number of samples
      sSmp :: ImageSample -> ImageSample
      sSmp (x, y, (WS w s)) = (x, y, (WS (w * f) s))
      f = 1 / (fromIntegral ppp)

oneRay :: Scene -> (ImageSample -> Rand m ()) -> Rand m ()
oneRay scene splat = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
       wo = normalize $ rayDir ray
   if (pdf > 0) && not (isBlack li)
      then nextVertex scene (-wo) (intersect scene ray) (sScale li (absDot nl wo / pdf)) 0 splat
      else return ()
      
connectCam :: Scene -> (ImageSample -> Rand s ()) -> Spectrum -> Bsdf -> Vector -> Float -> Rand s ()
connectCam sc splat li bsdf wi eps
   | isBlack f || isBlack csf || cPdf == 0 = return ()
   | sc `intersects` cray = return ()
   | otherwise = 
      splat $ (px, py, WS (absDot n we / (cPdf * dCam2)) (f * li * csf))
   where
      (CameraSampleResult csf pCam px py cPdf) = sampleCam (sceneCam sc) p
      dCam = pCam - p
      we = normalize $ dCam
      f = evalBsdf True bsdf wi we
      dCam2 = sqLen dCam
      cray = Ray p we eps (sqrt dCam2 - eps)
      n = normalize $ bsdfShadingNormal bsdf
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
         pcont = if depth > 3 then 0.5 else 1
         bsdf = intBsdf int
         n = normalize $ bsdfShadingNormal bsdf
         p = bsdfShadingPoint bsdf
         (BsdfSample _ spdf bf wo) = sampleBsdf bsdf wi ubc ubd
         li' = sScale (li * bf) (absDot n wo / (spdf * pcont))
         int' = intersect sc $ Ray p wo (intEpsilon int) infinity
         wi' = -wo
      
      connectCam sc splat li bsdf wi $ intEpsilon int

      if (isBlack bf) || spdf == 0
         then return ()
         else rnd >>= \x -> if x > pcont
                             then return ()
                             else nextVertex sc wi' int' li' (depth + 1) splat
                             
