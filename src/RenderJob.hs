
module RenderJob (
   Job, parseJob, ppJob, jobScene, jobIntegrator, imageSizeX, imageSizeY,
   samplesPerPixel
   ) where

import Camera
import Image
import Lafortune
import Light
import Material
import Math
import Pathtracer
import Plastic
import Primitive
import Scene
import Spectrum
import Texture
import Transform
import Transport
import TriangleMesh
import Whitted

import Control.Monad (liftM)
import Data.Array
import Debug.Trace
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
ppJob (MkJob sc _ flt spp sx sy) = PP.vcat [
   PP.text "Image size is" PP.<+> PP.text ((show sx) ++ "x" ++ (show sy)),
   PP.text "Pixel filter is" PP.<+> PP.text (show flt),
   PP.text "Samples per pixel is" PP.<+> PP.text (show spp),
   PP.text "Scene stats" PP.$$ PP.nest 3 (ppScene sc)
   ]
   
data PState = PState {
   _resX :: Int,
   _resY :: Int,
   pxFilter :: Filter, -- ^ the pixel filtering function
   camera :: Camera,
   transform :: Transform,
   material :: Material,
   _spp :: Int,
   emit :: Maybe Spectrum, -- ^ the emission for the next primitives
   lights :: [Light],
   prims :: [AnyPrim]
   }
   
startState :: PState
startState = PState 1024 768 Box
   (pinHoleCamera (View (mkV(3, 7, -6)) (mkV(0,0,0)) (mkV(0, 1, 0)) 1.8 (4.0/3.0)))
   identity
   (measuredMaterial BluePaint)
   2
   Nothing
   []
   []
   
parseJob :: String -> Job
parseJob s = either (error . show) (id) pr where
   pr = runParser jobParser startState "unknown source"  s
   
type JobParser a = GenParser Char PState a

jobParser :: JobParser Job
jobParser = do
   many object
   eof
   (PState sx sy flt cam _ _ spp _ ls ps) <- getState
   let scn = mkScene ls ps cam
   return (MkJob scn pathTracer flt spp sx sy)

sceneParser :: JobParser Scene
sceneParser = do
   many object
   eof
   s <- getState
   return (mkScene [] (prims s) (camera s))
   
object :: JobParser ()
object =
       do try comment
      <|> try pMesh
      <|> try pCamera
      <|> try pFilter
      <|> try pSize
      <|> try pSamplesPerPixel
      <|> try pEmission
      <|> try pLight
      <|> try pMaterial
      <|> pTransform
      <|> ws

--
-- parsing transformations
--

pTransform :: JobParser ()
pTransform = do
   pts <- between start end (many ts)
   return () where
      ts = choice [
         tIdentity,
         try tRotX,
         try tRotY,
         tRotZ,
         tScale,
         tTrans,
         ws
         ]
      start = string "beginTransform" >> ws
      end = string "endTransform"
   
tIdentity :: JobParser ()
tIdentity = do
   string "identity"
   s <- getState
   setState s { transform = identity }
   
tRotX :: JobParser ()
tRotX = do
   deg <- string "rotateX" >> ws >> flt
   s <- getState
   setState s { transform = concatTrans (transform s) (rotateX deg) }
   
tRotY :: JobParser ()
tRotY = do
   deg <- string "rotateY" >> ws >> flt
   s <- getState
   setState s { transform = concatTrans (transform s) (rotateY deg) }
   
tRotZ :: JobParser ()
tRotZ = do
   deg <- string "rotateZ" >> ws >> flt
   s <- getState
   setState s { transform = concatTrans (transform s) (rotateZ deg) }
   
tScale :: JobParser ()
tScale = do
   d <- string "scale" >> ws >> pVec
   s <- getState
   setState s { transform = concatTrans (transform s) (scale d) }
   
tTrans :: JobParser ()
tTrans = do
   d <- string "translate" >> ws >> pVec
   s <- getState
   setState s { transform = concatTrans (transform s) (translate d) }
   
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
   _ <- string "beginDirectionalLight\n"
   s <- pSpectrum <|> fail "missing spectrum"
   _ <- string "normal" <|> fail "missing normal"
   n <- pVec <|> fail "could not parse normal"
   _ <- char '\n'
   _ <- string "endDirectionalLight\n"
   return (Directional s (normalize n))
   
pEmission :: JobParser ()
pEmission = do
   _ <- string "beginEmission\n"

   spec <- do
      try (string "black\n" >> return Nothing)
      <|> (pSpectrum >>= (\s -> return (Just s)))
      
   _ <- string "endEmission\n"
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
   setState s { _resX = sx, _resY = sy }

-- | parses the pixel filtering function
pFilter :: JobParser ()
pFilter = do
   _ <- string "filter"
   _ <- spaces
   
   flt <- do
         try (string "box" >> return Box)
         <|> (string "sinc" >> return (Sinc 1 1 1))
         
   s <- getState
   setState s { pxFilter = flt }
   
pCamera :: JobParser ()
pCamera = do
   string "beginCamera\n"
   string "pos"
   pos <- pVec
   char '\n'
   
   string "lookAt"
   la <- pVec
   char '\n'
   
   string "up"
   up <- pVec
   char '\n'
   
   string "fov "
   fov <- flt
   char '\n'
   
   string "endCamera\n"
   let v = View pos la up fov 1
   
   oldState <- getState
   setState oldState {camera = pinHoleCamera v}
   
pSpectrum :: JobParser Spectrum
pSpectrum = do
   _ <- string "rgb" <|> fail "missing rgb"
   spaces
   r <- flt <|> fail "can't parse red"
   spaces
   g <- flt <|> fail "can't parse green"
   spaces
   b <- flt <|> fail "can't parse blue"
   _ <- char '\n' <|> fail "expected eol"
   return (fromRGB (r, g, b))
   
pMaterial :: JobParser ()
pMaterial = do
   _ <- string "beginMaterial\n"
   _ <- string "type"
   spaces
   t <- many (noneOf "\n")
   char '\n'
   m <- case t of
      "measured" -> do
         m <- pMeasuredMaterial
         return (measuredMaterial m)
         
      "plastic" -> pPlasticMaterial
      "matte" -> pMatteMaterial
      _ -> fail ("unknown material type " ++ t)
      
   _ <- string "endMaterial\n"
   s <- getState
   setState s { material = m }

pMatteMaterial :: JobParser Material
pMatteMaterial = do
   kd <- pTexture "kd"
   return (matteMaterial kd)
   
pPlasticMaterial :: JobParser Material
pPlasticMaterial = do
   kd <- pTexture "kd"
   ks <- pTexture "ks"
   rough <- namedFloat "rough"
   return (plasticMaterial kd ks rough)
   
pTexture :: String -> JobParser SpectrumTexture
pTexture n = do
   _ <- string "beginTexture"
   spaces
   _ <- string n
   _ <- char '\n'
   _ <- string "type"
   spaces
   tp <- many (noneOf "\n")
   char '\n'
   tx <- case tp of
      "constant" -> do
         s <- pSpectrum
         return (constantSpectrum s)
      _ -> fail ("unknown texture type " ++ tp)
   _ <- string "endTexture\n"
   return tx
   
pMeasuredMaterial :: JobParser Measured
pMeasuredMaterial = do
   _ <- string "name"
   spaces
   n <- many (noneOf "\n")
   char '\n'
   return (read n)
   
pMesh :: JobParser ()
pMesh = do
   _ <- string "beginMesh\n"
   vertexCount <- namedInt "vertexCount" <|> fail "vertexCount missing"
   faceCount <- namedInt "faceCount"  <|> fail "faceCount missing"
   vertices <- count vertexCount vertex
   let va = listArray (0, vertexCount-1) vertices
   faces <- count faceCount (face va)
   _ <- string "endMesh\n"
   s <- getState
   let mesh = mkMesh (material s) (emit s) (transform s) faces
   setState s {prims=[MkAnyPrim mesh] ++ prims s}

namedFloat :: String -> JobParser Flt
namedFloat n = do
   _ <- string n
   _ <- spaces
   res <- flt <|> fail ("cannot parse " ++ n ++ " value")
   _ <- char '\n'
   return res
   
namedInt :: String -> JobParser Int
namedInt n = do
   _ <- string n
   _ <- spaces
   res <- integ <|> fail ("cannot parse " ++ n ++ " value")
   _ <- char '\n'
   return res
   
face :: (Array Int Vertex) -> JobParser [Vertex]
face vs = do
   _ <- char 'f'
   is <- many1 (try (do _ <- (many (char ' ')); integ))
   _ <- char '\n'
   return (map (vs !) is)
   
pVec :: JobParser Vector
pVec = do
   spaces
   x <- flt
   spaces
   y <- flt
   spaces
   z <- flt
   return (MkVector x y z)
   
vertex :: JobParser Vertex
vertex = do
   _ <- char 'v'
   v <- pVec
   _ <- char '\n'
   return (Vertex v)

matrix :: Char -> JobParser [[Flt]]
matrix p = do
   m <- count 4 row
   return m where
      row = do
         _ <- char p
         r <- count 4 (try (do spaces; flt))
         _ <- char '\n'
         return r
   
-- | parse an integer
integ :: JobParser Int
integ = do
   x <- many1 digit
   return (read x)

-- | parse a floating point number
flt :: JobParser Flt
flt = do
  sign <- option 1 ( do s <- oneOf "+-"
                        return $ if s == '-' then (-1.0) else 1.0)
  i <- many digit
  d <- try (char '.' >> try (many digit))
  return $ sign * read (i++"."++d)

-- | skips over whitespace and comments
ws :: JobParser ()
ws = many1 (choice [space >> return (), comment]) >> return ()

comment :: JobParser ()
comment = do
   char '#' >> many (noneOf "\n") >> char '\n' >> return () <?> "comment"