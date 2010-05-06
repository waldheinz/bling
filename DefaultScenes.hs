
module DefaultScenes(glassSphere, plasticSpheres) where

import Camera
import Color
import Geometry
import Light
import Material
import Plastic
import Primitive
import Scene
import Specular
import Texture

gpMat :: Material
gpMat = plasticMaterial
   (graphPaper 0.08 (fromXyz (0.8, 0.8, 0.8)) (fromXyz (0.02, 0.02, 0.02)))
   (constantSpectrum $ fromXyz (0.5, 0.5, 0.5))
   0.02
   
plTest :: Float -> (Float, Float, Float) -> Material
plTest e kd  = plasticMaterial
   (constantSpectrum $ fromXyz kd)
   (constantSpectrum $ fromXyz (0.85, 0.85, 0.85))
   e

glassSphere :: Float -> Scene
glassSphere aspect = mkScene [] 
   [
      mkPrim (Sphere 1.1 (1, 1.1, 0)) (glassMaterial 1.5),
      mkPrim' (Sphere 1.5 (-2,5,2)) blackBodyMaterial (Just (fromXyz (35, 35, 35))),
      mkPrim (Plane (-0.0) (0, 1, 0)) gpMat
   ] 
   (pinHoleCamera (View (3, 3, -8) (2,0.5,0) (0, 1, 0) 1.8 aspect))

plasticSpheres :: Float -> Scene
plasticSpheres aspect = mkScene [SoftBox $ fromXyz (0.95, 0.95, 0.95)]
   [
      mkPrim (Sphere (0.9) (1, 1, -1)) (plTest 0.001 (1, 0.56, 0)),
      mkPrim (Sphere (0.9) (-1, 1, -1)) (plTest 0.01 (0.38, 0.05, 0.67)),
      mkPrim (Sphere (0.9) (-1, 1, 1)) (plTest 0.1 (1, 0.96, 0)),
      mkPrim (Sphere (0.9) (1, 1, 1)) (plTest 1 (0.04, 0.4, 0.64)),
      mkPrim (Plane (-0.1) (0, 1, 0)) gpMat
   ]
   (pinHoleCamera (View (3, 7, -6) (0,0.5,0) (0, 1, 0) 1.8 aspect))
