
module Graphics.Bling.IO.RenderJob (
   Job(..), parseJob, ppJob
   ) where

import Graphics.Bling.Filter
import Graphics.Bling.Integrator
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Transform
import Graphics.Bling.IO.CameraParser
import Graphics.Bling.IO.IntegratorParser
import Graphics.Bling.IO.LightParser
import Graphics.Bling.IO.MaterialParser
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.SamplerParser
import Graphics.Bling.IO.ShapeParser
import Graphics.Bling.IO.TransformParser

import Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint as PP

data Job = MkJob {
   jobScene :: Scene,
   jobIntegrator :: AnySurfaceIntegrator,
   jobSampler :: AnySampler,
   jobPixelFilter :: Filter,
   imageSizeX :: Int,
   imageSizeY :: Int
   }
   
ppJob :: Job -> PP.Doc
ppJob (MkJob sc int _ f sx sy) = PP.vcat [
   PP.text "Image size is" PP.<+> PP.text ((show sx) ++ "x" ++ (show sy)),
   PP.text "Pixel filter is" PP.<+> PP.text (show f),
   PP.text "Surface Integrator" PP.<+> pp int,
   PP.text "Scene stats" PP.$$ PP.nest 3 (ppScene sc)
   ]

startState :: PState
startState = PState 640 480 mkBoxFilter
   (defaultCamera 640 480)
   defaultSurfaceIntegrator
   identity
   defaultMaterial
   defaultSampler
   Nothing
   []
   []
   
parseJob :: String -> Job
parseJob s = either (error . show) (id) pr where
   pr = runParser jobParser startState "unknown source"  s

jobParser :: JobParser Job
jobParser = do
   _ <- many object
   eof
   (PState sx sy f cam i _ _ smp _ ls ps) <- getState
   let scn = mkScene ls ps cam
   return (MkJob scn i smp f sx sy)

object :: JobParser ()
object = 
       do try pShape
      <|> try pSurfaceIntegrator
      <|> try pCamera
      <|> pFilter
      <|> try pSize
      <|> try pEmission
      <|> try pLight
      <|> pMaterial
      <|> pTransform
      <|> ws

--  | parses the image size
pSize :: JobParser ()
pSize = do
   _ <- string "imageSize"
   _ <- spaces
   sx <- integ
   _ <- spaces
   sy <- integ
   s <- getState
   setState s { resX = sx, resY = sy }

-- | parses the pixel filtering function
pFilter :: JobParser ()
pFilter = do
   _ <- try (string "filter") >> ws
   
   t <- many1 alphaNum
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
