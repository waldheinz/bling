
import Graphics.Bling.Edsl

import Control.Monad
import System.Random

w = 1280
h = 720

--wall :: Transform -> IO ()
wall base = 
   forM_ [(x,z) | x <- [-20 .. 20], z <- [-20 .. 20]] $ \(x, z) -> do
      s <- io $ getStdRandom $ randomR (0.5, 1)
      setTransform $ scale (mkV' s (3*s) s) <> translate (mkV' x 0 z) <> base
      
      r <- io $ getStdRandom $ randomR (0.5, 0.95)
      g <- io $ getStdRandom $ randomR (0, 1)
      
      setMaterial $ mkMatte (const $ fromRGB' r r r) (const g)
      shape (mkBox (mkPoint (-1,-1,-1)) (mkPoint (1,1,1))) >>= add
   
core :: IO RenderJob
core = buildJob $ do
   setCamera $ mkPerspectiveCamera (lookAt (mkPoint' (-4) 6 (-8)) (mkPoint' 0 0 0) (mkV' 0 1 0)) 0 1 70
         (fromIntegral w) (fromIntegral h)
   setImageSize (w, h)
   setFilter $ mkMitchellFilter 3 3 0.333333 0.333333
   
   wall $ translate $ mkV' 0 (-20) 0
   wall $ translate $ mkV' 0   20  0
   wall $ rotateX 90 <> translate (mkV' 0  0  20)
   wall $ rotateZ 90 <> translate (mkV' 20  0  0)
   wall $ rotateZ 90 <> translate (mkV' (-20)  0  0)
   
   setTransform $ mempty
   shape (mkCylinder 0.5 (-20) 20 360) >>= add
   setTransform $ rotateX 90
   shape (mkCylinder 0.5 (-20) 20 360) >>= add
   setTransform $ rotateY 90
   shape (mkCylinder 0.5 (-20) 20 360) >>= add
   
   setTransform $ mempty
   emit $ fromRGB' 8 8 10
 --  shape (mkBox (mkPoint (-1,-1,-1)) (mkPoint (1,1,1))) >>= add
   
   shape (mkSphere 2) >>= add
   
   return () 

main :: IO ()
main = do
   let
      sampler = mkStratifiedSampler 6 6
      integrator = mkPathIntegrator 15 4
      renderer = mkSamplerRenderer sampler integrator

   core >>= renderWithPreview renderer

