
import Graphics.Bling.Edsl
import Graphics.Bling.Primitive
import Control.Monad
import System.IO
import Text.Printf

frames :: Int
frames = 1000

w = 640
h = 360

whiteM :: Material
whiteM = mkMatte (const $ fromRGB' 0.5 0.5 0.5) (const 0)

red :: Material
red = mkMatte (const $ fromRGB' 0.7 0.2 0.2) (const 0)

juliaJob :: Int -> IO RenderJob
juliaJob frame = buildJob $ do
   let
      t = fromIntegral frame / fromIntegral frames
      q = Quaternion (lerp t (0.450) (-0.2)) $ mkV (-0.447,lerp t (-0.5) 0.8, 0.306)
      j = mkFractalPrim (mkJuliaQuat q 0.0001 14) red
      ct = lookAt (mkPoint (-1.0, 1.5, -2.5)) (mkPoint (0, 0, 0)) (mkV' 0 1 0)
      sky = mkSunSkyLight (mkV' 0 0 1) (mkV' 0 0.15 (0.5)) 5

   setFilter $ mkMitchellFilter 3 3 0.333333 0.333333

   setImageSize (w, h)
   setCamera $ mkPerspectiveCamera ct 0 1 50 (fromIntegral w) (fromIntegral h)
--   add $ mkPointLight (fromRGB' 10 10 10) (mkPoint' 0 5 0)
--   add $ mkInfiniteAreaLight sky identity
   add $ mkAnyPrim j
   
   setMaterial whiteM
   setTransform $ concatTrans (rotateX 90) $ translate $ mkV (0, -1.4, 0)
   shape (mkQuad 50 50) >>= add
   
   emit $ fromRGB' 5 5 5
   setTransform $ translate $ mkV (0, 3, 0)
   shape (mkSphere 1) >>= add
   
main :: IO ()
main = do
   let
      sampler = mkStratifiedSampler 5 5
      integrator = mkPathIntegrator 3 2
      renderer = mkSamplerRenderer sampler integrator
      
   wnd <- mkPreviewWindow (w, h)
   
   forM_ [(1 :: Int) .. frames] $ \pass -> do
      
      juliaJob pass >>= \j -> render renderer j $ \prog -> do
         _ <- previewProgress wnd prog
         
         case prog of
            (PassDone p img spw) -> do
               let fname = "frame-" ++ printf "%05d" pass
               putStrLn $ "\nWriting " ++ fname ++ "..."
               h1 <- openFile (fname ++ ".ppm") WriteMode
               writePpm img spw h1
               hFlush h1
               hClose h1
               return False
            
            _ -> return True
         
