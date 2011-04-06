module Graphics.Bling.Scene (
   Scene, mkScene, scenePrim, sceneLights, sceneCam,
   Integrator, sampleOneLight, ppScene
   ) where

import Control.Monad
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Text.PrettyPrint

import Graphics.Bling.Bvh
import Graphics.Bling.Camera
import Graphics.Bling.Light as L
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Random
import Graphics.Bling.Spectrum
import Graphics.Bling.Transport

data Scene = Scene {
   scenePrim :: Bvh,
   sceneLights :: V.Vector Light,
   sceneCam :: Camera
   }
   
ppScene :: Scene -> Doc
ppScene (Scene p ls _) = vcat [
   text "bounds" <+> text (show (worldBounds p)),
   text "number of lights" <+> int (V.length ls),
   text "BVH stats" $$ nest 3 (ppBvh p)
   ]
   
mkScene :: (Primitive a) => [Light] -> [a] -> Camera -> Scene
mkScene l prims cam = Scene (mkBvh ps) (V.fromList lights) cam where
   lights = l ++ gl
   gl = mapMaybe light ps -- collect the geometric lights
   ps = Prelude.concatMap flatten prims
   
occluded :: Scene -> Ray -> Bool
occluded (Scene p _ _) = intersects p

type Integrator = Scene -> Ray -> Rand WeightedSpectrum
{-
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
-}

estimateDirect
   :: Scene
   -> Light
   -> Point
   -> Normal
   -> Vector
   -> Bsdf
   -> Rand Spectrum
{-# INLINE estimateDirect #-}
estimateDirect s l p n wo bsdf = do
   ul <- rnd2D
   let (LightSample li wi ray lpdf _) = sample l p n ul
   let f = (evalBsdf bsdf wo wi)
   if lpdf == 0 || isBlack li || isBlack f || occluded s ray
      then return black
      else return $ sScale (f * li) (absDot wi n * lpdf)
   
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
   | lc == 1 = estimateDirect scene (V.head lights)  p n wo bsdf
   | otherwise = liftM scale (estimateDirect scene (V.unsafeIndex lights ln) p n wo bsdf)
      where
            lc = V.length lights
            ln = min (floor $ ulNum * fromIntegral lc) (lc - 1)
            scale y = sScale y (fromIntegral lc)
            
