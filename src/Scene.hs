module Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam,
   Integrator, sampleOneLight, ppScene
   ) where

import Control.Monad
import Data.Array.Unboxed
import Data.Maybe (isJust, fromJust, mapMaybe)
import Text.PrettyPrint

import Bvh
import Camera
import Light as L
import Math
import Montecarlo
import Primitive
import Random
import Spectrum
import Transport

data Scene = Scene {
   scenePrim :: Bvh,
   sceneLights :: Array Int Light,
   sceneCam :: Camera
   }
   
ppScene :: Scene -> Doc
ppScene (Scene p ls _) = vcat [
   text "bounds" <+> text (show (worldBounds p)),
   text "number of lights" <+> int (rangeSize (bounds ls)),
   text "BVH stats" $$ nest 3 (ppBvh p)
   ]
   
mkScene :: (Primitive a) => [Light] -> [a] -> Camera -> Scene
mkScene l prims cam = Scene (mkBvh ps) la cam where
   la = listArray (0, length lights - 1) lights
   lights = l ++ gl
   gl = mapMaybe light ps -- collect the geometric lights
   ps = concatMap flatten prims
   
occluded :: Scene -> Ray -> Bool
occluded (Scene p _ _) = intersects p

type Integrator = Scene -> Ray -> Rand WeightedSpectrum

sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Normal -> Spectrum
sampleLightMis scene (LightSample li wi ray lpdf delta) bsdf wo n
   | lpdf == 0 || isBlack li || isBlack f || occluded scene ray = black
   | delta = sScale (f * li) (absDot wi n / lpdf)
   | otherwise = sScale (f * li) (absDot wi n * weight / lpdf)
   where
         f = evalBsdf bsdf wo wi
         weight = powerHeuristic (1, lpdf) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Normal -> Point -> Spectrum
sampleBsdfMis (Scene sp _ _) l (BsdfSample _ bPdf f wi) n p
   | isBlack f || bPdf == 0 = black
   | isJust lint = scale $ intLe (fromJust lint) (neg wi) -- TODO: need to check if the "right" light was hit
   | otherwise = scale (le l ray)
   where
         lPdf = pdf l p wi
         weight = powerHeuristic (1, bPdf) (1, lPdf)
         scale li = sScale (f * li) (absDot wi n * weight / bPdf)
         ray = Ray p wi epsilon infinity
         lint = sp `intersect` ray

estimateDirect
   :: Scene
   -> Light
   -> Point
   -> Normal
   -> Vector
   -> Bsdf
   -> Rand Spectrum

estimateDirect s l p n wo bsdf = do
   ul <- rnd2D
   let (LightSample li wi ray lpdf delta) = sample l p n ul
   if lpdf == 0 || occluded s ray
      then return black
      else return $ sScale ((evalBsdf bsdf wo wi) * li) (absDot wi n * lpdf)
   
{-
estimateDirect s l p n wo bsdf = do
   uL <- rnd2D
   uBC <- rnd
   uBD <- rnd2D
   let ls = sampleLightMis s (sample l p n uL) bsdf wo n
   let bs = black -- sampleBsdfMis s l (sampleBsdf bsdf wo uBC uBD) n p
   return (ls + bs)
   -}
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Float -> Rand Spectrum
sampleOneLight scene@(Scene _ lights _) p n wo bsdf ulNum
   | lc == 0 = return black
   | lc == 1 = estimateDirect scene (lights ! 0)  p n wo bsdf
   | otherwise = liftM scale (estimateDirect scene (lights ! ln) p n wo bsdf)
      where
            lc = snd (bounds lights) + 1
            ln = min (floor $ ulNum * fromIntegral lc) (lc - 1)
            scale y = sScale y (fromIntegral lc)
            
