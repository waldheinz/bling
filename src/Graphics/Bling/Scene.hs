module Graphics.Bling.Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam, sampleOneLight,
   sampleLightRay, lightPower, scIntersect, occluded,
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
import Graphics.Bling.Primitive.KdTree

data Scene = Scene
   { _scenePrimCount :: ! Int     -- just for reference
   , _sceneAccel     :: ! KdTree
   , scenePrim       :: ! Primitive
   , sceneLights     :: ! (V.Vector Light)
   , sceneCam        :: ! Camera
   }
   
instance Printable Scene where
   prettyPrint (Scene cnt a p ls cam) = vcat [
      text "camera" <+> prettyPrint cam,
      text "bounds" <+> text (show (worldBounds p)),
      text "number of lights" <+> int (V.length ls),
      text "number of primitives" <+> int cnt,
      text "stats" $$ nest 3 (ppKdTree a)
      ]

mkScene :: [Light] -> [Primitive] -> Camera -> Scene
mkScene l prims cam = Scene cnt accel prim (V.fromList lights) cam where
   lights = l ++ gl
   prim = kdTreePrimitive accel
   accel = mkKdTree ps
   gl = mapMaybe light ps -- collect the geometric lights
   ps = Prelude.concatMap flatten prims
   cnt = length ps
   
occluded :: Scene -> Ray -> Bool
{-# INLINE occluded #-}
occluded (Scene _ _ p _ _) = intersects p

scIntersect :: Scene -> Ray -> Maybe Intersection
{-# INLINE scIntersect #-}
scIntersect (Scene _ _ p _ _) = intersect p

-- | total power of all light sources in the scene in [Watt]
lightPower :: Scene -> Spectrum
lightPower s = V.sum $ V.map (\l -> L.power l (worldBounds $ scenePrim s)) (sceneLights s)

--------------------------------------------------------------------------------
-- Sampling Scene Light Sources
--------------------------------------------------------------------------------

sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Spectrum
{-# INLINE sampleLightMis #-}
sampleLightMis scene (LightSample li wi ray lpdf delta) bsdf wo
   | lpdf == 0 || isBlack li || isBlack f || occluded scene ray = black
   | delta = sScale (f * li) (1 / lpdf)
   | otherwise = sScale (f * li) (weight / lpdf)
   where
         f = evalBsdf False bsdf wo wi
         weight = powerHeuristic (1, lpdf) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Point -> Float -> Spectrum
{-# INLINE sampleBsdfMis #-}
sampleBsdfMis (Scene _ _ sp _ _) l (BsdfSample _ bPdf f wi) p epsilon
   | bPdf == 0 || isBlack f = black
   | isJust lint = maybe black ff $ intLight (fromJust lint)
   | otherwise = sc (le l ray)
   where
         ff l' = if l' == l then sc $ intLe (fromJust lint) (-wi) else black
         lPdf = pdf l p wi
         sc li = sScale (f * li) $ powerHeuristic (1, bPdf) (1, lPdf)
         ray = Ray p wi epsilon infinity
         lint = sp `intersect` ray

estimateDirect
   :: Scene
   -> Light
   -> Point
   -> Float
   -> Normal
   -> Vector
   -> Bsdf
   -> RandLightSample
   -> Spectrum
{-# INLINE estimateDirect #-}
estimateDirect s l p eps n wo bsdf smp = ls + bs where
   ls = {-# SCC "estimateDirect.light" #-} sampleLightMis s (sample l p eps n $ ulDir smp) bsdf wo
   bs = {-# SCC "estimateDirect.bsdf"  #-} sampleBsdfMis s l (sampleBsdf bsdf wo uBC uBD) p eps
   uBC = uBsdfComp smp
   uBD = uBsdfDir smp
   
-- | the random values needed to sample a light source in the scene
data RandLightSample = RLS
   { ulNum     :: {-# UNPACK #-} !Float
   , ulDir     :: {-# UNPACK #-} !R.Rand2D
   , uBsdfComp :: {-# UNPACK #-} !Float
   , uBsdfDir  :: {-# UNPACK #-} !R.Rand2D
   }
   
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Float -> Normal -> Vector -> Bsdf -> RandLightSample -> Spectrum
sampleOneLight scene@(Scene _ _ _ lights _) p eps n wo bsdf smp
   | lc == 0 = black
   | lc == 1 = {-# SCC "sampleOneLight.single" #-} ed (V.head lights)
   | otherwise = {-# SCC "sampleOneLight.many" #-} sScale ld (fromIntegral lc) where     
            ld = ed $ V.unsafeIndex lights ln
            ed l = estimateDirect scene l p eps n wo bsdf smp
            lc = V.length lights
            ln = min (floor $ (ulNum smp) * fromIntegral lc) (lc - 1)

-- | samples an outgoing light ray from a randomly chosen light source
sampleLightRay
   :: Scene
   -> Float
   -> R.Rand2D
   -> R.Rand2D
   -> (Spectrum, Ray, Normal, Float)

sampleLightRay (Scene _ _ p ls _) uL uO uD
   | V.null ls = (black, undefined, undefined, 0)
   | lc == 1 = sample' (V.head ls) (worldBounds p) uO uD
   | otherwise = (s, ray, n, pd / fromIntegral lc)
   where
      (s, ray, n, pd) = sample' (V.unsafeIndex ls ln) (worldBounds p) uO uD
      lc = V.length ls
      ln = min (floor $ uL * fromIntegral lc) (lc - 1)
      
