
module RenderJob (
   Job, parseJob, ppJob, jobScene, jobIntegrator, imageSizeX, imageSizeY
   ) where

import Camera
import Image
import Lafortune
import Light
import Material
import Math
import Pathtracer
import Primitive
import Scene
import Spectrum
import Texture
import Transform
import Transport
import TriangleMesh

import Data.Array
import Text.ParserCombinators.Parsec
import qualified Text.PrettyPrint as PP

data Job = MkJob {
   jobScene :: Scene,
   jobIntegrator :: Integrator,
   jobPixelFilter :: Filter,
   imageSizeX :: Int,
   imageSizeY :: Int
   }
   
ppJob :: Job -> PP.Doc
ppJob (MkJob sc _ flt sx sy) = PP.vcat [
   PP.text "Image size is" PP.<+> PP.text ((show sx) ++ "x" ++ (show sy)),
   PP.text "Pixel filter is" PP.<+> PP.text (show flt),
   PP.text "Scene stats" PP.$$ PP.nest 3 (ppScene sc)
   ]
   
data PState = PState {
   _resX :: Int,
   _resY :: Int,
   pxFilter :: Filter, -- ^ the pixel filtering function
   camera :: Camera,
   prims :: [AnyPrim]
   }
   
startState :: PState
startState = PState 1024 768 Box
   (pinHoleCamera (View (mkV(3, 7, -6)) (mkV(0,0,0)) (mkV(0, 1, 0)) 1.8 (4.0/3.0)))
   []
   
parseJob :: String -> Job
parseJob s = either (error . show) (id) pr where
   pr = runParser jobParser startState "unknown source"  s
   
type JobParser a = GenParser Char PState a

jobParser :: JobParser Job
jobParser = do
   many object
   eof
   (PState sx sy flt cam ps) <- getState
   let scn = (mkScene [SoftBox $ fromRGB (0.95, 0.95, 0.95)] ps cam)
   return (MkJob scn pathTracer flt sx sy)

sceneParser :: JobParser Scene
sceneParser = do
   many object
   eof
   s <- getState
   return (mkScene [SoftBox $ fromRGB (0.95, 0.95, 0.95)] (prims s) (camera s))
   
object :: JobParser ()
object =
       do try comment
      <|> try mesh
      <|> try cam
      <|> try pFilter
      <|> try pSize
      <|> do try (char '\n'); return ()

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
   
cam :: JobParser ()
cam = do
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
   spaces
   r <- flt
   spaces
   g <- flt
   spaces
   b <- flt
   return (fromRGB (r, g, b))

pMaterial :: JobParser Material
pMaterial = do
   string "beginMaterial\n"
   string "diffuse"
   ds <- pSpectrum
   char '\n'
   string "endMaterial\n"
   return  (measuredMaterial Primer) --  (matteMaterial (constantSpectrum ds))
   
mesh :: JobParser ()
mesh = do
   string "mesh"
   vertexCount <- try (do spaces; integ)
   faceCount <- try (do spaces; integ)
   char '\n'
   m <- matrix 'm'
   i <- matrix 'i'
   mat <- pMaterial
   vertices <- count vertexCount vertex
   let va = listArray (0, vertexCount-1) vertices
   faces <- count faceCount (face va)
   oldState <- getState
   let mesh = mkMesh mat (fromMatrix (m, i)) faces
   setState oldState {prims=[MkAnyPrim mesh] ++ prims oldState}
   
face :: (Array Int Vertex) -> JobParser [Vertex]
face vs = do
   char 'f'
   indices <- many1 (try (do (many (char ' ')); integ))
   char '\n'
   return (map (vs !) indices)
   
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
   char 'v'
   v <- pVec
   char '\n'
   return (Vertex v)

matrix :: Char -> JobParser [[Flt]]
matrix p = do
   m <- count 4 row
   return m where
      row = do
         char p
         r <- count 4 (try (do spaces; flt))
         char '\n'
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

comment :: JobParser ()
comment = do
   char '#'
   many (noneOf "\n")
   char '\n'
   return ()
