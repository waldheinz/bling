
module DefaultScenes(glassSphere) where

import Camera
import Color
import Geometry
import Image
import Light
import Material
import Plastic
import Primitive
import Scene
import Specular
import Texture

gpMat :: Material
gpMat = plasticMaterial
   (graphPaper 0.08 (fromXyz (0.04, 0.4, 0.64)) (fromXyz (0.05, 0.05, 0.05)))
   (constantSpectrum $ fromXyz (0.1, 0.1, 0.1))
   0.2

plTest :: Float -> (Float, Float, Float) -> Material
plTest e kd  = plasticMaterial
   (constantSpectrum $ fromXyz kd)
   (constantSpectrum $ fromXyz (0.85, 0.85, 0.85))
   e

myShape :: Primitive
myShape = Group [
   mkPrim (Sphere 1.1 (1, 1.1, 0)) (glassMaterial 1.5),
   mkPrim' (Sphere 1.5 (-2,5,2)) blackBodyMaterial (Just (fromXyz (35, 35, 35))),
   mkPrim (Plane (-0.0) (0, 1, 0)) gpMat
   ]

glassSphere :: Float -> Scene
glassSphere aspect = mkScene [] 
   [myShape] 
   (pinHoleCamera (View (3, 3, -8) (2,0.5,0) (0, 1, 0) 1.8 aspect))
