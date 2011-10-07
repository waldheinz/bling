
module Graphics.Bling.Integrator.LightTracer (

   LightTracer, mkLightTracer
   
   ) where

-- import Control.Monad
-- import Control.Monad.ST
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Primitive
-- import Graphics.Bling.Random
import Graphics.Bling.Reflection
-- import Graphics.Bling.Rendering
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum
import Graphics.Bling.Sampling

data LightTracer = LightT
   { _maxDepth :: Int
   , _sampleDepth :: Int
   }

mkLightTracer
   :: Int
   -> Int
   -> LightTracer
mkLightTracer = LightT

instance Printable LightTracer where
   prettyPrint (LightT _ _) = PP.vcat [
      PP.text "light tracer" {-,
      PP.int np PP.<+> PP.text "passes",
      PP.int ppp PP.<+> PP.text "photons per pass" -} ]


smps2D :: Int
smps2D = 3

smps1D :: Int
smps1D = 4

smp2doff :: Int -> Int
smp2doff d = smps2D * d

smp1doff :: Int -> Int
smp1doff d = smps1D * d

instance SurfaceIntegrator LightTracer where
   sampleCount1D (LightT _ sd) = smps1D * sd

   sampleCount2D (LightT _ sd) = smps2D * sd

   contrib (LightT md _) scene addsmp _ = {-# SCC "lightContrib" #-} do
      ul <- rnd
      ulo <- rnd2D
      uld <- rnd2D
      let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
      let wo = normalize $ rayDir ray
      nextVertex scene (-wo) (intersect scene ray) (sScale li (absDot nl wo / pdf)) 0 splat
      where
         splat is = liftSampled $ addsmp (True, is)
{-      
   render (LightT np ppp maxD smpD) job report = pass np img where
      sc = jobScene job
      img = mkJobImage job
      pass n i
         | n == 0 = return ()
         | otherwise = do
            seed <- ioSeed

            img' <- stToIO $ do
               mimg <- thaw i
               runWithSeed seed $ replicateM_ ppp $ oneRay sc (liftR . (splatSample mimg) . sSmp)
               freeze mimg
               
            cont <- report $ (PassDone (np - n + 1) img')
            if cont
               then pass (n - 1) img'
               else return ()
      
      -- | scales an image sample according to total light power in scene
      --   and total number of samples
      sSmp :: ImageSample -> ImageSample
      sSmp (ImageSample x y (w, s)) = ImageSample x y (w * f, s)
      f = 1 / (fromIntegral $ np * ppp)
      
oneRay :: Scene -> (ImageSample -> Rand m ()) -> Rand m ()
oneRay scene splat = do
   ul <- rnd
   ulo <- rnd2D
   uld <- rnd2D
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
   let wo = normalize $ rayDir ray
   nextVertex scene (-wo) (intersect scene ray) (sScale li (absDot nl wo / pdf)) 0 splat
   -}
nextVertex
   :: Scene
   -> Vector
   -> Maybe Intersection
   -> Spectrum
   -> Int -- ^ depth
   -> (ImageSample -> Sampled m ())
   -> Sampled m ()
-- nothing hit, terminate path   
nextVertex _ _ Nothing _ _ _ = return ()

nextVertex sc wi (Just int) li depth splat
   | isBlack li = return ()
   | otherwise = do
   let (CameraSampleResult csf pCam px py cPdf) = sampleCam (sceneCam sc) p
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
               then return ()
	       else nextVertex sc wi' int' li' (depth + 1) splat
   
   if cPdf > 0 && not (isBlack (f * li)) && not (intersects sc cray)
      then splat smpHere >> rest
      else rest
      
   where
      pcont = if depth > 3 then 0.5 else 1
      bsdf = intBsdf int
      n = bsdfShadingNormal bsdf
      p = bsdfShadingPoint bsdf
      
