
module Graphics.Bling.IO.RenderJob (
   parseJob
   ) where

import System.FilePath
import Text.Parsec.Error

import Graphics.Bling.Filter
import Graphics.Bling.Rendering
import Graphics.Bling.Scene
import Graphics.Bling.Transform
import Graphics.Bling.IO.CameraParser
import Graphics.Bling.IO.LightParser
import Graphics.Bling.IO.MaterialParser
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.PrimitiveParser
import Graphics.Bling.IO.RendererParser
import Graphics.Bling.IO.TransformParser

startState :: FilePath -> PState
startState base = PState 640 480 defaultRenderer mkBoxFilter
   (defaultCamera 640 480)
   identity
   defaultMaterial
   Nothing
   []
   []
   0
   base
   
parseJob :: FilePath -> IO (Either ParseError (RenderJob, AnyRenderer))
parseJob fname = do
   file <- readFile fname
   runPT jobParser (startState $ takeDirectory fname)fname file

jobParser :: JobParser (RenderJob, AnyRenderer)
jobParser = do
   _ <- many object
   optional ws >> eof
   (PState sx sy r f cam _ _ _ ls ps _ _) <- getState
   let scn = mkScene ls ps cam
   return (mkJob scn f sx sy, r)

object :: JobParser ()
object = do
   objName <- try $ optional ws >> pString
   ws
   
   case objName of
        "filter" -> pFilter
        "prim" -> do -- a top-level primitive
            p <- pPrimitive
            s <- getState
            setState s {prims = p : (prims s)}
        "imageSize" -> pSize
        "renderer" -> pRenderer
        "transform" -> pTransform
        "camera" -> pCamera
        "light" -> pLight
        "material" -> pMaterial
        "emission" -> pEmission
        _ -> fail $ "unknwon object type " ++ objName

--  | parses the image size
pSize :: JobParser ()
pSize = do
   sx <- integ
   _ <- spaces
   sy <- integ
   s <- getState
   setState s { resX = sx, resY = sy }

-- | parses the pixel filtering function
pFilter :: JobParser ()
pFilter = do
   t <- pString
   _ <- ws
   f <- case t of
      "box" -> return mkBoxFilter
      
      "sinc" -> do
         xw <- flt
         yw <- ws >> flt
         tau <- ws >> flt
         return (mkSincFilter xw yw tau)
         
      "triangle" -> do
         xw <- flt
         yw <- ws >> flt
         return (mkTriangleFilter xw yw)

      "mitchell" -> do
         xw <- flt
         yw <- ws >> flt
         b <- ws >> flt
         c <- ws >> flt
         return (mkMitchellFilter xw yw b c)
         
      _ -> fail ("unknown pixel filter function \"" ++ t ++ "\"")
   
   s <- getState
   setState s { pxFilter = f }
