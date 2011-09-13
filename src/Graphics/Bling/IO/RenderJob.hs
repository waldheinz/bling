
module Graphics.Bling.IO.RenderJob (
   Job(..), parseJob
   ) where

import Graphics.Bling.Filter
import Graphics.Bling.Rendering
import Graphics.Bling.Scene
import Graphics.Bling.Transform
import Graphics.Bling.Types
import Graphics.Bling.IO.CameraParser
import Graphics.Bling.IO.LightParser
import Graphics.Bling.IO.MaterialParser
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.RendererParser
import Graphics.Bling.IO.ShapeParser
import Graphics.Bling.IO.TransformParser

import Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint as PP

data Job = MkJob {
   jobScene :: Scene,
   jobRenderer :: AnyRenderer,
   jobPixelFilter :: Filter,
   imageSizeX :: Int,
   imageSizeY :: Int
   }
   
instance Printable Job where
   prettyPrint (MkJob sc r f sx sy) = PP.vcat [
      PP.text "image size" PP.<+> PP.text ((show sx) ++ "x" ++ (show sy)),
      PP.text "pixel filter" PP.<+> PP.text (show f),
      PP.text "renderer " PP.<+> prettyPrint r,
      PP.text "scene" PP.$$ PP.nest 3 (prettyPrint sc)
      ]
   
startState :: PState
startState = PState 640 480 defaultRenderer mkBoxFilter
   (defaultCamera 640 480)
   identity
   defaultMaterial
   Nothing
   []
   []
   0
   
parseJob :: String -> Job
parseJob s = either (error . show) (id) pr where
   pr = runParser jobParser startState "unknown source"  s

jobParser :: JobParser Job
jobParser = do
   _ <- many object
   optional ws >> eof
   (PState sx sy r f cam _ _ _ ls ps _) <- getState
   let scn = mkScene ls ps cam
   return (MkJob scn r f sx sy)

object :: JobParser ()
object = do
   objName <- try $ optional ws >> pString
   ws
   
   case objName of
        "shape" -> pShape
        "filter" -> pFilter
        "imageSize" -> pSize
        "renderer" -> pRenderer
        "transform" -> pTransform
        "camera" -> pCamera
        "light" -> pLight
        "material" -> pMaterial
        _ -> fail $ "unknwon object type " ++ objName
{- 
object = 
       do try pShape
      <|> try pRenderer
      <|> try pCamera
      <|> pFilter
      <|> try pSize
      <|> try pEmission
      <|> try pLight
      <|> pMaterial
      <|> pTransform
      <|> pFractal
      <|> ws
      -}
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
