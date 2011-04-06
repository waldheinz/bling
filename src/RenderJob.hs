
module RenderJob (
   Job, parseJob, ppJob,
   jobScene, jobIntegrator, imageSizeX, imageSizeY, jobPixelFilter,
   samplesPerPixel
   ) where

import Camera
import Filter
import Lafortune
import Light
import Material
import Math
import Pathtracer
import Plastic
import Primitive
import Scene
import Shape
import Specular
import Spectrum
import Texture
import Transform

import qualified Data.Vector as V
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
   
data PState = PState {
   resX :: Int,
   resY :: Int,
   pxFilter :: Filter, -- ^ the pixel filtering function
   camera :: Camera,
   transform :: Transform,
   material :: Material,
   _spp :: Int,
   emit :: Maybe Spectrum, -- ^ the emission for the next primitives
   lights :: [Light],
   prims :: [AnyPrim]
   }
   
aspect :: PState -> Flt
aspect s = (fromIntegral (resX s)) / (fromIntegral (resY s))
   
startState :: PState
startState = PState 1024 768 mkBoxFilter
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
   _ <- many object
   eof
   (PState sx sy f cam _ _ spp _ ls ps) <- getState
   let scn = mkScene ls ps cam
   return (MkJob scn pathTracer f spp sx sy)

object :: JobParser ()
object = 
       do pShape
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
-- parsing shapes
--

pShape :: JobParser ()
pShape = do
   try (string "beginShape") >> ws
   _ <- string "type" >> ws
   t <- many alphaNum
   ws
   sh <- case t of
      "sphere" -> do
         r <- namedFloat "radius"
         return [mkSphere r]
      "mesh" -> pMesh
      
      _ -> fail ("unknown shape type \"" ++ t ++ "\"")
   _ <- ws >> string "endShape"
   
   s <- getState
   let p = map (mkGeom (transform s) False (material s) (emit s)) sh
   setState s {prims = map MkAnyPrim p ++ (prims s)}

pMesh :: JobParser [Shape]
pMesh = do
   vc <- namedInt "vertexCount" <|> fail "vertexCount missing"
   ws
   fc <- namedInt "faceCount"  <|> fail "faceCount missing"
   vertices <- count vc vertex
   let va = V.fromList vertices
   faces <- count fc (face va)
   return (triangulate faces)
   
face :: (V.Vector Vertex) -> JobParser [Vertex]
face vs = do
   _ <- ws >> char 'f'
   is <- many1 (try (do ws; integ))
   return (map (vs V.!) is)

vertex :: JobParser Vertex
vertex = do
   _ <- ws >> char 'v'
   v <- ws >> pVec
   return (Vertex v)

--
-- parsing transformations
--

pTransform :: JobParser ()
pTransform = between start end (many ts) >> return () where
   ts = choice [
      tIdentity, try tRotX,
      try tRotY,
      tRotZ,
      tScale,
      tTrans,
      tMatrix,
      ws]
   start = try (string "beginTransform")
   end = string "endTransform"
   
tIdentity :: JobParser ()
tIdentity = do
   _ <- string "identity"
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

tMatrix :: JobParser ()
tMatrix = do
   _ <- try (string "beginMatrix")
   m <- mtr 'm'
   i <- mtr 'i'
   _ <- ws >> string "endMatrix"
   let t = fromMatrix (m, i)
   s <- getState
   setState s { transform = concatTrans (transform s) t }

mtr :: Char -> JobParser [[Flt]]
mtr p = count 4 row where
   row = do
      _ <- ws >> char p
      r <- count 4 (try (do ws; flt))
      return r
   
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
pCamera = do
   _ <- try (string "beginCamera")
   _ <- ws >> string "pos"
   pos <- ws >> pVec
   _ <- ws >> string "lookAt"
   la <- ws >> pVec
   _ <- ws >> string "up"
   up <- ws >> pVec
   _ <- ws >> string "fov"
   fov <- ws >> flt
   _ <- ws >> string "endCamera"
   s <- getState
   setState s {camera = pinHoleCamera (View pos la up fov (aspect s))}
   
pSpectrum :: JobParser Spectrum
pSpectrum = do
   _ <- try (string "rgb")
   r <- ws >> flt
   g <- ws >> flt
   b <- ws >> flt
   return (fromRGB (r, g, b))

--
-- parsing materials
--

pMaterial :: JobParser ()
pMaterial = do
   _ <- try (string "beginMaterial")
   ws >> string "type" >> ws
   t <- many alphaNum
   m <- case t of
      "measured" -> do
         m <- pMeasuredMaterial
         return (measuredMaterial m)
         
      "plastic" -> pPlasticMaterial
      "matte" -> pMatteMaterial
      "mirror" -> pMirrorMaterial
      
      _ -> fail ("unknown material type " ++ t)
      
   _ <- ws >> string "endMaterial"
   s <- getState
   setState s { material = m }

pMirrorMaterial :: JobParser Material
pMirrorMaterial = do
   r <- ws >> pSpectrum
   return (mirrorMaterial r)

pMatteMaterial :: JobParser Material
pMatteMaterial = do
   kd <- pTexture "kd"
   return (matteMaterial kd)
   
pPlasticMaterial :: JobParser Material
pPlasticMaterial = do
   kd <- pTexture "kd"
   ks <- pTexture "ks"
   rough <- ws >> namedFloat "rough"
   return (plasticMaterial kd ks rough)
   
pTexture :: String -> JobParser SpectrumTexture
pTexture n = do
   ws >> string "beginTexture" >> ws >> string n >> ws >> string "type" >> ws
   tp <- many alphaNum
   ws
   tx <- case tp of
      "constant" -> do
         s <- pSpectrum
         return (constant s)
      _ -> fail ("unknown texture type " ++ tp)
   _ <- ws >> string "endTexture"
   return tx
   
pMeasuredMaterial :: JobParser Measured
pMeasuredMaterial = do
   _ <- string "name" >> ws
   n <- many alphaNum
   return (read n)
   
namedFloat :: String -> JobParser Flt
namedFloat n = do
   _ <- string n >> ws
   res <- flt <|> fail ("cannot parse " ++ n ++ " value")
   return res
   
namedInt :: String -> JobParser Int
namedInt n = do
   _ <- string n
   _ <- spaces
   res <- integ <|> fail ("cannot parse " ++ n ++ " value")
   _ <- char '\n'
   return res

pVec :: JobParser Vector
pVec = do
   x <- flt
   y <- ws >> flt
   z <- ws >> flt
   return (MkVector x y z)
   
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
