
module Graphics.Bling.Integrator.LightTracer (

   LightTracer, mkLightTracer
   
   ) where

import Control.Monad
import Control.Monad.ST
import Debug.Trace
import qualified Text.PrettyPrint as PP

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
   prettyPrint (LightT np ppp) = PP.vcat [
      PP.text "light tracer",
      PP.int np PP.<+> PP.text "passes",
      PP.int ppp PP.<+> PP.text "photons per pass" ]

instance Renderer LightTracer where
   render (LightT np ppp) sc img report = pass np where
      pass n
         | n == 0 = return ()
         | otherwise = do
            smps <- runRandIO $ liftM concat $ replicateM ppp $ oneRay sc
            stToIO $ mapM_ ((splatSample img) . sSmp) smps
         
            cont <- report $ Progress (PassDone (np - n + 1)) img
            if cont
               then pass (n - 1)
               else return ()
      
      -- | scales an image sample according to total light power in scene
      --   and total number of samples
      sSmp :: ImageSample -> ImageSample
      sSmp (ImageSample x y (w, s)) = ImageSample x y (w * f, s)
      f = 1 -- 10000000 / cnt
      cnt = fromIntegral $ np * ppp
      
oneRay :: Scene -> Rand [ImageSample]
oneRay scene = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
   let wo = normalize $ rayDir ray
   nextVertex scene (-wo) (intersect scene ray) (sScale li (absDot nl wo / pdf)) 0
   
nextVertex
   :: Scene
   -> Vector
   -> Maybe Intersection
   -> Spectrum
   -> Int -- ^ depth
   -> Rand [ImageSample]
-- nothing hit, terminate path   
nextVertex _ _ Nothing _ _ = return []

nextVertex sc wi (Just int) li depth
   | isBlack li = return []
   | otherwise = do
   uc <- rnd2D
   let (CameraSample csf pCam px py cPdf) = sampleCam (sceneCam sc) p uc
   let dCam = pCam - p
   let we = normalize $ dCam
   let f = evalBsdf bsdf wi we
   let dCam2 = sqLen dCam
   let smpHere = ImageSample px py (absDot n we / (cPdf * dCam2), f * li * csf)
   
   ubc <- rnd
   ubd <- rnd2D
      
   let (BsdfSample _ spdf bf wo) = sampleBsdf bsdf wi ubc ubd
   let li' = sScale (li * bf) (absDot n wo / (spdf * pcont))
   let int' = intersect sc $ Ray p wo epsilon infinity
   let wi' = -wo
   let cray = segmentRay pCam p
   
   x <- rnd
   let rest = if x > pcont
               then return []
               else nextVertex sc wi' int' li' (depth + 1)
   
   if cPdf > 0 && not (isBlack (f * li)) && not (intersects sc cray)
      then (liftM . (:)) smpHere $! rest
      else rest
      
   where
      pcont = if depth > 3 then 0.5 else 1
      dg = intGeometry int
      bsdf = intBsdf int
      n = normalize $ dgN dg
      p = dgP dg
      
