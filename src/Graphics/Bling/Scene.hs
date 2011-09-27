module Graphics.Bling.Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam, sampleOneLight,
   sampleLightRay, lightPower,
      RandLightSample(..)
   ) where

import Data.Maybe (mapMaybe, isJust, fromJust)
import qualified Data.Vector as V
import Text.PrettyPrint

import Graphics.Bling.Camera
import Graphics.Bling.Light as L
import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Primitive
import qualified Graphics.Bling.Random as R
import Graphics.Bling.Reflection
import Graphics.Bling.Spectrum
import Graphics.Bling.Primitive.KdTree

data Scene = Scene {
   _scenePrimCount :: Int, -- just for reference
   scenePrim :: KdTree,
   sceneLights :: V.Vector Light,
   sceneCam :: Camera
   }
   
instance Printable Scene where
   prettyPrint (Scene cnt p ls cam) = vcat [
      text "camera" <+> prettyPrint cam,
      text "bounds" <+> text (show (worldBounds p)),
      text "number of lights" <+> int (V.length ls),
      text "number of primitives" <+> int cnt,
      text "stats" $$ nest 3 (ppKdTree p)
      ]

mkScene :: (Primitive a) => [Light] -> [a] -> Camera -> Scene
mkScene l prims cam = Scene cnt (mkKdTree ps) (V.fromList lights) cam where
   lights = l ++ gl
   gl = mapMaybe light ps -- collect the geometric lights
   ps = Prelude.concatMap flatten prims
   cnt = length ps
   
occluded :: Scene -> Ray -> Bool
occluded (Scene _ p _ _) = intersects p

instance Primitive Scene where
   intersect = intersect.scenePrim
   intersects = intersects.scenePrim
   worldBounds = worldBounds.scenePrim
   flatten = flatten.scenePrim
   light = light.scenePrim
   shadingGeometry = shadingGeometry.scenePrim

-- | total power of all light sources in the scene in [Watt]
lightPower :: Scene -> Spectrum
lightPower s = V.sum $ V.map (\l -> L.power l (worldBounds s)) (sceneLights s)

--------------------------------------------------------------------------------
-- Sampling Scene Light Sources
--------------------------------------------------------------------------------

sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Normal -> Spectrum
{-# INLINE sampleLightMis #-}
sampleLightMis scene (LightSample li wi ray lpdf delta) bsdf wo n
   | lpdf == 0 || isBlack li || isBlack f || occluded scene ray = black
   | delta = sScale (f * li) (absDot wi n / lpdf)
   | otherwise = sScale (f * li) (absDot wi n * weight / lpdf)
   where
         f = evalBsdf bsdf wo wi
         weight = powerHeuristic (1, lpdf) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Normal -> Point -> Spectrum
{-# INLINE sampleBsdfMis #-}
sampleBsdfMis (Scene _ sp _ _) l (BsdfSample _ bPdf f wi) n p
   | isBlack f || bPdf == 0 = black
   | isJust lint = maybe black ff $ intLight (fromJust lint)
   | otherwise = sc (le l ray)
   where
         ff l' = if l' == l then sc $ intLe (fromJust lint) (-wi) else black
         lPdf = pdf l p wi
         weight = powerHeuristic (1, bPdf) (1, lPdf)
         sc li = sScale (f * li) (absDot wi n * weight / bPdf)
         ray = Ray p wi epsilon infinity
         lint = sp `intersect` ray

estimateDirect
   :: Scene
   -> Light
   -> Point
   -> Normal
   -> Vector
   -> Bsdf
   -> RandLightSample
   -> Spectrum
{-# INLINE estimateDirect #-}
estimateDirect s l p n wo bsdf smp = ls where
   ls = {-# SCC "estimateDirect.light" #-} sampleLightMis s (sample l p n $ ulDir smp) bsdf wo n
   bs = {-# SCC "estimateDirect.bsdf"  #-} sampleBsdfMis s l (sampleBsdf bsdf wo uBC uBD) n p
   uBC = uBsdfComp smp
   uBD = uBsdfDir smp
   
-- | the random values needed to sample a light source in the scene
data RandLightSample = RLS
   { ulNum ::  {-# UNPACK #-} ! Flt
   , ulDir :: {-# UNPACK #-} ! R.Rand2D
   , uBsdfComp :: {-# UNPACK #-} ! Flt
   , uBsdfDir :: {-# UNPACK #-} ! R.Rand2D
   }
   
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> RandLightSample -> Spectrum
sampleOneLight scene@(Scene _ _ lights _) p n wo bsdf smp
   | lc == 0 = black
   | lc == 1 = ed (V.head lights)
   | otherwise = sScale ld (fromIntegral lc) where     
            ld = ed $ V.unsafeIndex lights ln
            ed l = {-# SCC "sampleOneLight.estimate" #-} estimateDirect scene l p n wo bsdf smp
            lc = V.length lights
            ln = min (floor $ (ulNum smp) * fromIntegral lc) (lc - 1)

-- | samples an outgoing light ray from a randomly chosen light source
sampleLightRay
   :: Scene
   -> Flt
   -> R.Rand2D
   -> R.Rand2D
   -> (Spectrum, Ray, Normal, Flt)

sampleLightRay sc@(Scene _ _ ls _) uL uO uD
   | V.null ls = (black, undefined, undefined, 0)
   | lc == 1 = sample' (V.head ls) (worldBounds sc) uO uD
   | otherwise = (s, ray, n, pd')
   where
      (s, ray, n, pd) = sample' (V.unsafeIndex ls ln) (worldBounds sc) uO uD
      pd' = pd * fromIntegral lc
      lc = V.length ls
      ln = min (floor $ uL * fromIntegral lc) (lc - 1)
