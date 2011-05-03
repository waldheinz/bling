
module Graphics.Bling.IO.ShapeParser (
   
   pShape, pFractal
   
   ) where

import qualified Data.Vector as V
import Text.ParserCombinators.Parsec

import Graphics.Bling.Primitive
import Graphics.Bling.Shape
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.Primitive.Fractal

--
-- parsing fractals
--

pFractal :: JobParser ()
pFractal = flip namedBlock "fractal" $ do
   t <- many alphaNum
   f <- case t of
             "julia" -> do
                c <- ws >> pNamedQuat "c"
                e <- ws >> namedFloat "epsilon"
                i <- ws >> namedInt "iterations"
                return $ mkJuliaQuat c e i
             _ -> fail $ "unknown fractal type " ++ t

   s <- getState
   let p = mkFractalPrim f $ material s
   setState s {prims = (mkAnyPrim p) : (prims s)}

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

pShape :: JobParser ()
pShape = (flip namedBlock) "shape" $ do
   t <- many alphaNum
   ws
   sh <- case t of
      "sphere" -> do
         r <- namedFloat "radius"
         return [mkSphere r]
         
      "mesh" -> pMesh
      
      _ -> fail $ "unknown shape type \"" ++ t ++ "\""
   
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
   
