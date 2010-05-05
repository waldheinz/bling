module Scene where

import Control.Monad
import Maybe (isJust, fromJust)

import Color
import Light
import Math
import Primitive
import Random
import Transport

data Scene = Scene {
   scenePrimitive :: Primitive,
   sceneLights :: [Light]
   }

occluded :: Scene -> Ray -> Bool
occluded (Scene p _) = primIntersects p
   
type Integrator = Scene -> Ray -> Rand WeightedSpectrum
   
evalLight :: Scene -> Point -> Normal -> Light -> Vector -> Bsdf -> Rand2D -> Spectrum
evalLight scene p n light wo bsdf us = (evalSample scene sample wo bsdf p n) where
   sample = lightSample light p n us
   
evalSample :: Scene -> LightSample -> Vector -> Bsdf -> Point -> Normal -> Spectrum
evalSample scene sample wo bsdf _ n
   | isBlack li || isBlack f = black
   | occluded scene (testRay sample) = black
   | otherwise = sScale (f * li) $ (absDot wi n) / lPdf
   where
         lPdf = lightSamplePdf sample
         li = de sample
         wi = lightSampleWi sample
         f = evalBsdf bsdf wo wi
   
sampleLightMis :: Scene -> LightSample -> Bsdf -> Vector -> Normal -> Spectrum
sampleLightMis scene (LightSample li wi ray pdf deltaLight) bsdf wo n
   | (pdf == infinity) || (isBlack li) || (isBlack f) || (occluded scene ray) = black
   | deltaLight = sScale (f * li) ((absDot wi n) / pdf)
   | otherwise = sScale (f * li) ((absDot wi n) * weight / pdf)
   where
         f = evalBsdf bsdf wo wi
         weight = powerHeuristic (1, pdf) (1, bsdfPdf bsdf wo wi)

sampleBsdfMis :: Scene -> Light -> BsdfSample -> Normal -> Point -> Spectrum
sampleBsdfMis (Scene sp _) light (BsdfSample _ bPdf f wi) n p
   | (isBlack f) || (bPdf == infinity) = black
   | isJust lint = scale $ intLe (fromJust lint) (neg wi) -- TODO: need to check if the "right" light was hit
   | otherwise = scale (lightEmittance light ray)
   where
         lPdf = lightPdf light p n wi
         weight = powerHeuristic (1, bPdf) (1, lPdf)
         scale = (\li -> sScale (f * li) ((absDot wi n) * weight / bPdf))
         ray = Ray p wi epsilon infinity
         lint = primIntersect sp ray
         
-- | samples all lights by sampling individual lights and summing up the results
sampleAllLights :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleAllLights scene p n wo bsdf = undefined

estimateDirect :: Scene -> Light -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
estimateDirect s l p n wo bsdf = 
   let lSmp = lightSample l p n
       bSmp = sampleBsdf bsdf wo
   in do
   uL <- rnd2D
   uBC <- rnd
   uBD <- rnd2D
   return $ (sampleLightMis s (lSmp uL) bsdf wo n) + (sampleBsdfMis s l (bSmp uBC uBD) n p)
   
-- | samples one randomly chosen light source
sampleOneLight :: Scene -> Point -> Normal -> Vector -> Bsdf -> Rand Spectrum
sampleOneLight (Scene _ []) _ _ _ _ = return black -- no light sources -> no light
sampleOneLight scene@(Scene _ (l:[])) p n wo bsdf =
   estimateDirect scene l p n wo bsdf
sampleOneLight scene@(Scene _ lights) p n wo bsdf = do
   lightNumF <-rndR (0, fromIntegral lightCount)
   lightNum <- return $ floor lightNumF
   y <- estimateDirect scene (lights !! lightNum) p n wo bsdf
   return $! scale y where
      lightCount = length lights
      scale = (\y -> sScale y (fromIntegral lightCount))
