
module Graphics.Bling.IO.RenderJob (
   Job, parseJob, ppJob,
   jobScene, jobIntegrator, imageSizeX, imageSizeY, jobPixelFilter,
   samplesPerPixel
   ) where

import Graphics.Bling.Camera
import Graphics.Bling.Filter
import Graphics.Bling.Light
import Graphics.Bling.Math
import Graphics.Bling.Pathtracer
import Graphics.Bling.Scene
import Graphics.Bling.Transform
import Graphics.Bling.IO.MaterialParser
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.ShapeParser
import Graphics.Bling.IO.TransformParser

import Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint as PP

data Job = MkJob {
   jobScene :: Scene,
   jobIntegrator :: Integrator,
   jobPixelFilter :: Filter,
   samplesPerPixel :: Int,
   imageSizeX :: Int,
   imageSizeY :: Int
   }
   
ppJob :: Job -> PP.Doc
ppJob (MkJob sc _ f spp sx sy) = PP.vcat [
   PP.text "Image size is" PP.<+> PP.text ((show sx) ++ "x" ++ (show sy)),
   PP.text "Pixel filter is" PP.<+> PP.text (show f),
   PP.text "Samples per pixel is" PP.<+> PP.text (show spp),
   PP.text "Scene stats" PP.$$ PP.nest 3 (ppScene sc)
   ]

aspect :: PState -> Flt
aspect s = (fromIntegral (resX s)) / (fromIntegral (resY s))
   
startState :: PState
startState = PState 1024 768 mkBoxFilter
   (pinHoleCamera (View (mkV(3, 7, -6)) (mkV(0,0,0)) (mkV(0, 1, 0)) 1.8 (4.0/3.0)))
   identity
   defaultMaterial
   2
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
   (PState sx sy f cam _ _ spp _ ls ps) <- getState
   let scn = mkScene ls ps cam
   return (MkJob scn pathTracer f spp sx sy)

object :: JobParser ()
object = 
       do try pShape
      <|> pCamera
      <|> pFilter
      <|> try pSize
      <|> try pSamplesPerPixel
      <|> try pEmission
      <|> try pLight
      <|> pMaterial
      <|> pTransform
      <|> ws

--
-- parsing light sources
--

pLight :: JobParser ()
pLight = do
   l <- pDirectionalLight
   s <- getState
   setState s { lights = l : (lights s) }

pDirectionalLight :: JobParser Light
pDirectionalLight = do
   try (string "beginDirectionalLight") >> ws
   s <- pSpectrum  <|> fail "missing spectrum"
   _ <- ws >> (string "normal" <|> fail "missing normal")
   n <- ws >> (pVec <|> fail "could not parse normal")
   _ <- ws >> string "endDirectionalLight"
   return (mkDirectional s n)
   
pEmission :: JobParser ()
pEmission = do
   try (string "beginEmission") >> ws
   spec <- do
      try (string "black" >> return Nothing)
      <|> (pSpectrum >>= (\s -> return (Just s)))
      
   ws >> string "endEmission" >> ws
   s <- getState
   setState s { emit = spec }
   
pSamplesPerPixel :: JobParser ()
pSamplesPerPixel = do
   spp <- namedInt "samplesPerPixel"
   s <- getState
   setState s { _spp = spp }

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
   
pCamera :: JobParser ()
pCamera = (flip namedBlock) "camera" $ do
   pos <- namedVector "pos"
   la <- ws >> namedVector "lookAt"
   up <- ws >> namedVector "up"
   fov <- ws >> namedFloat "fov"
   s <- getState
   setState s {camera = pinHoleCamera (View pos la up fov (aspect s))}
