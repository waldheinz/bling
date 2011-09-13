
module Graphics.Bling.IO.CameraParser (
   defaultCamera, pCamera
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Camera
import Graphics.Bling.Math
import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore

defaultCamera :: Int -> Int -> Camera
defaultCamera sx sy =
   mkPerspectiveCamera c2w 0 1 90 (fromIntegral sx) (fromIntegral sy) where
      c2w = translate (Vector 0 0 (-5))

pCamera :: JobParser ()
pCamera = pBlock $ do
   t <- pString
   s <- getState
   
   cam <- case t of
               "perspective" -> pPerspectiveCam s
               "environment" -> pEnvironmentCam s
               _ -> fail $ "unknown camera type " ++ t
   
   setState s { camera = cam }

pPerspectiveCam :: PState -> JobParser Camera
pPerspectiveCam s = do
   fov <- ws >> namedFloat "fov"
   lr <- ws >> namedFloat "lensRadius"
   fd <- ws >> namedFloat "focalDistance"
   let sx = fromIntegral $ resX s
   let sy = fromIntegral $ resY s
   let c2w = transform s
   return $ mkPerspectiveCamera c2w lr fd fov sx sy
   
pEnvironmentCam :: PState -> JobParser Camera
pEnvironmentCam s = return $ mkEnvironmentCamera c2w sx sy where
   sx = fromIntegral $ resX s
   sy = fromIntegral $ resY s
   c2w = transform s
   
