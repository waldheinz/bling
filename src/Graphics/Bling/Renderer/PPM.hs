
module Graphics.Bling.Renderer.PPM (

   ProgressivePhotonMap, mkProgressivePhotonMap
   
   ) where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Camera
import Graphics.Bling.Image
import Graphics.Bling.Primitive
import qualified Graphics.Bling.Random as R
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum

data ProgressivePhotonMap = PPM

instance Printable ProgressivePhotonMap where
   prettyPrint (PPM) = PP.vcat [
      PP.text "Progressive Photon Map" ]

mkProgressivePhotonMap :: ProgressivePhotonMap
mkProgressivePhotonMap = PPM

data HitPoint = Hit
   { hpBsdf    :: {-# UNPACK #-} ! Bsdf
   , hpPixel   :: {-# UNPACK #-} ! (Flt, Flt)
   , hpW       :: {-# UNPACK #-} ! Vector
   , hpR2      :: {-# UNPACK #-} ! Flt
   }

alpha :: Flt
alpha = 0.7

mkHitPoints :: Scene -> Image -> R.Rand s [HitPoint]
mkHitPoints scene img = do
   let
      cam = sceneCam scene

   result <- R.newRandRef []
      
   forM_ (splitWindow $ imageWindow' img) $ \w -> do
      sample (mkRandomSampler 1) w 0 0 $ do
         p <- fireRay cam >>= traceRay scene
         liftSampled $ modifySTRef result (p++)

   R.readRandRef result

traceRay :: Scene -> Ray -> Sampled s [HitPoint]
traceRay scene ray = maybe (return $! []) ls (scene `intersect` ray) where
   ls int = do
--      ubc <- rnd
--      ubd <- rnd2D
      let
         bsdf = intBsdf int
--          p = bsdfShadingPoint bsdf
--          n = bsdfShadingNormal bsdf
         wo = -(rayDir ray)

      -- trace rays for specular reflection and transmission
--      refl <- cont (d+1) md s bsdf wo $ mkBxdfType [Specular, Reflection]
--      trans <- cont (d+1) md s bsdf wo $ mkBxdfType [Specular, Transmission]
      pxpos <- do
         cs <- cameraSample
         return (imageX cs, imageY cs)
         
      return $! [Hit bsdf pxpos wo (20*20)]

lookupHits :: [HitPoint] -> Point -> Normal -> [HitPoint]
lookupHits ps p n = filter f ps where
   f hit = n `dot` hpn > 0 && sqLen v <= hpR2 hit where
      hpn = bsdfShadingNormal $ hpBsdf hit
      v = (bsdfShadingPoint $ hpBsdf hit) - p

tracePhoton :: Scene -> [HitPoint] -> MImage s -> R.Rand s ()
tracePhoton scene hps img = do
   ul <- R.rnd
   ulo <- R.rnd2D
   uld <- R.rnd2D
   let (li, ray, nl, pdf) = sampleLightRay scene ul ulo uld
       wo = normalize $ rayDir ray
       splat = addContrib img
   if (pdf > 0) && not (isBlack li)
      then nextVertex scene hps (-wo) (intersect scene ray) (sScale li (absDot nl wo / pdf)) 0 splat
      else return ()

nextVertex :: Scene -> [HitPoint] -> Vector -> Maybe Intersection -> Spectrum -> Int -> ((Bool, ImageSample) -> ST s ()) -> R.Rand s ()
nextVertex _ _ _ Nothing _ _ _ = return ()
nextVertex scene hps wi (Just int) li d splat = do
   
   -- add contribution for this photon hit
   let
      bsdf = intBsdf int
      p = bsdfShadingPoint bsdf
      n = bsdfShadingNormal bsdf
      hps' = lookupHits hps p n

   forM_ hps' $ \hp -> do
      let
         (px, py) = hpPixel hp
         hpbsdf = hpBsdf hp
         f = evalBsdf hpbsdf (hpW hp) wi
      R.liftR $ splat $ (True, ImageSample px py (1 / (pi * pi * hpR2 hp), f * li))

   -- follow the path
   ubc <- R.rnd
   ubd <- R.rnd2D
   let
      (BsdfSample _ spdf f wo) = sampleBsdf bsdf wi ubc ubd
      pcont = if d > 3 then 0.5 else 1
      li' = sScale (f * li) (absDot wo n / (spdf * pcont))
      ray = Ray p wo epsilon infinity
      
   if (spdf == 0) || isBlack li'
      then return ()
      else R.rnd >>= \x -> if x > pcont
         then return ()
         else nextVertex scene hps (-wo) (scene `intersect` ray) li' (d+1) splat

instance Renderer ProgressivePhotonMap where
   render (PPM) job report = do
      seed <- R.ioSeed
      let
         img = mkJobImage job
         scene = jobScene job
      
      hitPoints <- stToIO $ R.runWithSeed seed $ mkHitPoints scene img

      currImg <- stToIO $ thaw img
      
      forM_ [1..] $ \passNum -> do
         pseed <- R.ioSeed
         stToIO $ R.runWithSeed pseed $ replicateM_ 100 $ tracePhoton scene hitPoints $ currImg
         img' <- stToIO $ freeze currImg
         _ <- report $ PassDone passNum img' (1 / fromIntegral passNum)
         return ()
         
      return ()

