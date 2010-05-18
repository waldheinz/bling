
module DefaultScenes(glassSphere, plasticSpheres, sphereCube, skyLightTest, blackBodyScene, boxScene) where

import Camera
import Spectrum
import Geometry
import Lafortune
import Light
import Material
import Plastic
import Primitive
import Scene
import Specular
import Texture
import TriangleMesh

import Debug.Trace

gpMat :: Spectrum -> Material
gpMat c = plasticMaterial
   (graphPaper 0.08 c (fromRGB (0.02, 0.02, 0.02)))
   (constantSpectrum $ fromRGB (0.5, 0.5, 0.5))
   0.02
   
plTest :: Float -> (Float, Float, Float) -> Material
plTest e kd  = plasticMaterial
   (constantSpectrum $ fromRGB kd)
   (constantSpectrum $ fromRGB (0.85, 0.85, 0.85))
   e

boxScene :: Float -> Scene
boxScene aspect = trace (show faces) $ mkScene [SoftBox $ fromRGB (0.95, 0.95, 0.95)] 
   [box , mkPrim (Plane 1.5 ( 0,  1,  0)) (measuredMaterial BrushedMetal)]
   (pinHoleCamera (View (7, 7, -7) (0,0,0) (0, 1, 0) 1.8 aspect))
   where
         box = Group $ map (\g -> mkPrim g (measuredMaterial BluePaint)) faces
         faces = f1 ++ f2 ++ f3 ++ f4  ++ f5 ++ f6
         f1 = triangulate [v1, v2, v3, v4] -- right face
         f2 = triangulate [v5, v7, v8, v6] -- left face
         f3 = triangulate [v1, v5, v6, v2] -- top face
         f4 = triangulate [v3, v7, v8, v4] -- 
         f5 = triangulate [v1, v5, v7, v3]
         f6 = triangulate [v2, v6, v8, v4]
         v1 = Vertex ( 1,  1,  1)
         v2 = Vertex ( 1,  1, -1)
         v3 = Vertex ( 1, -1,  1)
         v4 = Vertex ( 1, -1, -1)
         v5 = Vertex (-1,  1,  1)
         v6 = Vertex (-1,  1, -1)
         v7 = Vertex (-1, -1,  1)
         v8 = Vertex (-1, -1, -1)

blackBodyScene :: Float -> Scene
blackBodyScene aspect = mkScene []
   [  Group emitters , 
      mkPrim (Plane 0 ( 0,  1,  0)) (measuredMaterial BrushedMetal),
      mkPrim (Plane 2 ( 0,  0,  -1)) (measuredMaterial BluePaint)
   ]
   (pinHoleCamera (View (0, 3, -10) (0,0,0) (0, 1, 0) 1.8 aspect)) where
      emitters = map (\(p, t) -> mkPrim' (Sphere 0.4 p) blackBodyMaterial (Just $ sBlackBody t)) pts
      pts = zip poss temps
      poss = [(x, 0.5, 0) | x <- [-3..3]]
      temps = [1000, 2000 ..] 
   
skyLightTest :: Float -> Scene
skyLightTest aspect = mkScene [ mkProbeLight TestProbe ]
   [
      mkPrim (Sphere 1 (0, 1, 0)) (plTest 0.1 (0.9, 0.9, 0.9)),
      mkPrim (Plane 0 ( 0,  1,  0)) (gpMat $ fromXyz (0.8, 0.8, 0.8))
   ]
   (pinHoleCamera (View (3, 2, -7) (0,1,0) (0, 1, 0) 1.8 aspect))
   
sphereCube :: Float -> Scene
sphereCube aspect = mkScene [ ]
   [  Group spheres,
      mkPrim' (Sphere 1.5 (0,0,0)) blackBodyMaterial (Just $ fromXyz (15,3,3)),
      mkPrim (Plane pd ( 0,  1,  0)) (measuredMaterial Primer),
      mkPrim (Plane pd ( 0, -1,  0)) (measuredMaterial Primer),
      mkPrim (Plane pd ( 0,  0,  1)) (measuredMaterial Primer),
      mkPrim (Plane pd ( 0,  0, -1)) (measuredMaterial Primer),
      mkPrim (Plane pd ( 1,  0,  0)) (measuredMaterial Primer),
      mkPrim (Plane pd (-1,  0,  0)) (measuredMaterial Primer)
   ]
   (pinHoleCamera (View (3, 10, -10) (0,0.0,0) (0, 1, 0) 1.8 aspect)) where
      spheres = map (\pos -> mkPrim (Sphere r pos) (plTest 0.02  (0.9, 0.9, 0.9))) coords
      coords = filter (\(x, y, z) -> (abs x > 1) || (abs y > 1) || (abs z > 1)) coords'
      coords' = [(x,y,z) | x <- [(-cnt)..cnt], y <- [(-cnt)..cnt], z <- [(-cnt)..cnt]]
      cnt = 2
      r = 0.4
      pd = 11
    --  spMat = plTest 0.01 (0.9, 0.9, 0.9)
      
glassSphere :: Float -> Scene
glassSphere aspect = mkScene [] 
   [
      mkPrim (Sphere 1.1 (1, 1.1, 0)) (glassMaterial 1.5 white),
      mkPrim' (Sphere 1.5 (-2,5,2)) blackBodyMaterial (Just (fromXyz (35, 35, 35))),
      mkPrim (Plane (-0.0) (0, 1, 0)) (gpMat $ fromXyz (0.4, 0.5, 0.8))
   ] 
   (pinHoleCamera (View (3, 3, -8) (2,0.5,0) (0, 1, 0) 1.8 aspect))

plasticSpheres :: Float -> Scene
plasticSpheres aspect = mkScene [SoftBox $ fromRGB (0.95, 0.95, 0.95)]
   [
      mkPrim (Sphere (0.9) (1, 1, -1)) (plTest 0.001 (1, 0.56, 0)),
      mkPrim (Sphere (0.9) (-1, 1, -1)) (plTest 0.01 (0.38, 0.05, 0.67)),
      mkPrim (Sphere (0.9) (-1, 1, 1)) (plTest 0.1 (1, 0.96, 0)),
      mkPrim (Sphere (0.9) (1, 1, 1)) (plTest 1 (0.04, 0.4, 0.64)),
      mkPrim (Plane (-0.1) (0, 1, 0)) (gpMat $ fromRGB (0.8, 0.8, 0.8))
   ]
   (pinHoleCamera (View (3, 7, -6) (0,0.5,0) (0, 1, 0) 1.8 aspect))
