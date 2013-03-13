
module Graphics.Bling.IO.PrimitiveParser (
   
   pShape, pPrimitive
   
   ) where

import Control.Monad
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.Material
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.TriangleMesh
import Graphics.Bling.Shape
import Graphics.Bling.IO.MaterialParser
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.TransformParser
import Graphics.Bling.IO.WaveFront
import Graphics.Bling.Primitive.Bezier
import Graphics.Bling.Primitive.Fractal
import Graphics.Bling.Primitive.Geometry
import Graphics.Bling.Primitive.Heightmap

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------

pPrimitive :: JobParser [Primitive]
pPrimitive = pBlock $ do
   t <- pString
   p <- case t of
      "bezier" -> do
         subdivs <- namedInt "subdivs"
         patches <- many1 (try pBezierPatch)
         optional ws
         s <- getState
         return $ tesselateBezier subdivs patches (transform s) (material s)
      
      "heightMap" -> do
         ns <- integ
         nt <- integ
         elev <- pScalarMap2d
         tr <- pTransform
         s <- getState
         return $! heightMap elev (ns, nt) (material s) tr
         
      "julia" -> do
         c <- pNamedQuat "c"
         e <- namedFloat "epsilon"
         i <- namedInt "iterations"
         s <- getState
         return $! [mkJuliaQuat (material s) c e i]
      
      "mandelbulb" -> do
         order <- namedFloat "order"
         e <- namedFloat "epsilon"
         i <- namedInt "iterations"
         s <- getState
         return $ [mkMandelBulb (material s) order i e]
      
      "mesh" -> pMesh >>= \m -> return $! m
         
      "shape" -> do
         shp <- pShape
         s <- getState
         sid <- nextId
         return $ [mkGeom (transform s) False (material s) (emit s) shp sid]
         
      "waveFront" -> do
         fname <- pQString
         mmap <- pNamedMaterialMap "materials"
         parseWaveFront mmap fname
      
      _ -> fail $ "unknown primitive type " ++ t

   return $ p

pNamedMaterialMap :: String -> JobParser MaterialMap
pNamedMaterialMap n = (flip namedBlock) n $ do
   mats <- many $ do
      mn <- pQString
      m <- ws >> pBlock pMaterial'
      return (mn, m)
      
   s <- getState
   return $ mkMaterialMap (material s) mats

pNamedQuat :: String -> JobParser Quaternion
pNamedQuat n = string n >> ws >> pQuaternion

pQuaternion :: JobParser Quaternion
pQuaternion = liftM2 Quaternion flt pVec

--
-- parsing shapes
--

pShape :: JobParser Shape
pShape = pBlock $ do
   pString >>= \t -> case t of
      "box" -> liftM2 mkBox (namedVector "pmin") (namedVector "pmax")
      "cylinder" -> do
         r <- namedFloat "radius"
         zmin <- namedFloat "zmin"
         zmax <- namedFloat "zmax"
         phiMax <- namedFloat "phiMax"
         return $ mkCylinder r zmin zmax phiMax

      "disk" -> do
         h <- namedFloat "height"
         r <- namedFloat "radius"
         ri <- namedFloat "innerRadius"
         phiMax <- namedFloat "phiMax"
         return $ mkDisk h r ri phiMax

      "quad" -> liftM2 mkQuad flt flt
      "sphere" -> liftM mkSphere $ namedFloat "radius"

      _ -> fail $ "unknown shape type " ++ t
      
pBezierPatch :: JobParser Patch
pBezierPatch = (flip namedBlock) "p" $ do
   xs <- flt `sepBy` (optional ws >> char ',' >> optional ws)
   optional ws
   
   case mkPatch xs of
        (Right p) -> return p
        (Left err) -> fail $ "error parsing bezier patch: " ++ err

pMesh :: JobParser [Primitive]
pMesh = do
   vc <- namedInt "vertexCount" <?> "vertexCount missing"
   fc <- namedInt "faceCount"  <?> "faceCount missing"
   vs <- count vc $ char 'v' >> ws >> pVec
   fs <- fmap triangulate $ count fc $ char 'f' >> ws >> many1 (try integ)
   t <- currentTransform
   m <- currentMaterial
   return $! mkTriangleMesh t m (V.fromList vs) (V.fromList fs) Nothing Nothing 
   
