
module Graphics.Bling.IO.PrimitiveParser (
   
   pShape, pPrimitive
   
   ) where

import qualified Data.Vector.Unboxed as V

import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.TriangleMesh
import Graphics.Bling.Shape
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.WaveFront
import Graphics.Bling.Primitive.Bezier
import Graphics.Bling.Primitive.Fractal

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------

pPrimitive :: JobParser AnyPrim
pPrimitive = pBlock $ do
   t <- pString
   p <- case t of
      "bezier" -> do
         subdivs <- ws >> namedInt "subdivs"
         patches <- many1 (try pBezierPatch)
         optional ws
         s <- getState
         return $ mkAnyPrim $ tesselateBezier subdivs patches (transform s) (material s)
                
      "julia" -> do
         c <- ws >> pNamedQuat "c"
         e <- ws >> namedFloat "epsilon"
         i <- ws >> namedInt "iterations"
         s <- getState
         return $ mkAnyPrim $ mkFractalPrim (mkJuliaQuat c e i) (material s)
                
      "menger" -> do
         shape <- ws >> pShape
         level <- ws >> namedInt "level"
         s <- getState
         return $ mkAnyPrim $ mkMengerSponge shape (material s) level (transform s)
         
      "mesh" -> fmap mkAnyPrim pMesh
         
      "shape" -> do
         shp <- ws >> pShape
         s <- getState
         sid <- nextId
         return $ mkAnyPrim $ mkGeom (transform s) False (material s) (emit s) shp sid
         
      "waveFront" -> do
         fname <- ws >> pQString
         m <- parseWaveFront fname
         return $ mkAnyPrim m

      _ -> fail $ "unknown fractal type " ++ t

   return $ p

pNamedQuat :: String -> JobParser Quaternion
pNamedQuat n = do
   string n >> ws
   pQuaternion

pQuaternion :: JobParser Quaternion
pQuaternion = do
   r <- flt
   i <- ws >> pVec
   return $ Quaternion r i

--
-- parsing shapes
--

pShape :: JobParser Shape
pShape = pBlock $ do
   t <- pString
   
   case t of
      "box" -> do
         pmin <- ws >> namedVector "pmin"
         pmax <- ws >> namedVector "pmax"
         return $ mkBox pmin pmax

      "cylinder" -> do
         r <- ws >> namedFloat "radius"
         zmin <- ws >> namedFloat "zmin"
         zmax <- ws >> namedFloat "zmax"
         phiMax <- ws >> namedFloat "phiMax"
         return $ mkCylinder r zmin zmax phiMax

      "disk" -> do
         h <- ws >> namedFloat "height"
         r <- ws >> namedFloat "radius"
         ri <- ws >> namedFloat "innerRadius"
         phiMax <- ws >> namedFloat "phiMax"
         return $ mkDisk h r ri phiMax

      "quad" -> do
         sx <- ws >> flt
         sz <- ws >> flt
         return $ mkQuad sx sz
         
      "sphere" -> do
         r <- ws >> namedFloat "radius"
         return $ mkSphere r

      _ -> fail $ "unknown shape type " ++ t
      
pBezierPatch :: JobParser Patch
pBezierPatch = (flip namedBlock) "p" $ do
   xs <- flt `sepBy` (optional ws >> char ',' >> optional ws)
   optional ws
   
   case mkPatch xs of
        (Right p) -> return p
        (Left err) -> fail $ "error parsing bezier patch: " ++ err

pMesh :: JobParser TriangleMesh
pMesh = do
   vc <- ws >> (namedInt "vertexCount" <|> fail "vertexCount missing")
   fc <- ws >> (namedInt "faceCount"  <|> fail "faceCount missing")
   
   vs <- count vc vertex
   fs <- fmap concat $ count fc face
   
   t <- currentTransform
   m <- currentMaterial
   
   return $ mkTriangleMesh t m (V.fromList vs) (V.fromList fs) Nothing Nothing 
   
face :: JobParser [Int]
face = do
   _ <- ws >> char 'f'
   count 3 (ws >> integ)

vertex :: JobParser Point
vertex = do
   _ <- ws >> char 'v'
   ws >> pVec
   