
module Graphics.Bling.IO.PrimitiveParser (
   
   pShape, pPrimitive
   
   ) where

import Control.Monad
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.TriangleMesh
import Graphics.Bling.Shape
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.WaveFront
import Graphics.Bling.Primitive.Bezier
import Graphics.Bling.Primitive.Fractal
import Graphics.Bling.Primitive.Geometry

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------

pPrimitive :: JobParser AnyPrim
pPrimitive = pBlock $ do
   t <- pString
   p <- case t of
      "bezier" -> do
         subdivs <- namedInt "subdivs"
         patches <- many1 (try pBezierPatch)
         optional ws
         s <- getState
         return $ mkAnyPrim $ tesselateBezier subdivs patches (transform s) (material s)
                
      "julia" -> do
         c <- pNamedQuat "c"
         e <- namedFloat "epsilon"
         i <- namedInt "iterations"
         s <- getState
         return $ mkAnyPrim $ mkFractalPrim (mkJuliaQuat c e i) (material s)
                
      "menger" -> do
         shape <- pShape
         level <- namedInt "level"
         s <- getState
         return $ mkAnyPrim $ mkMengerSponge shape (material s) level (transform s)
         
      "mesh" -> fmap mkAnyPrim pMesh
         
      "shape" -> do
         shp <- pShape
         s <- getState
         sid <- nextId
         return $ mkAnyPrim $ mkGeom (transform s) False (material s) (emit s) shp sid
         
      "waveFront" -> fmap mkAnyPrim $ pQString >>= parseWaveFront
      _ -> fail $ "unknown fractal type " ++ t

   return $ p

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
         phiMax <- ws >> namedFloat "phiMax"
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

pMesh :: JobParser TriangleMesh
pMesh = do
   vc <- namedInt "vertexCount" <|> fail "vertexCount missing"
   fc <- namedInt "faceCount"  <|> fail "faceCount missing"
   vs <- count vc $ char 'v' >> ws >> pVec
   fs <- fmap triangulate $ count fc $ char 'f' >> ws >> many1 (try integ)
   t <- currentTransform
   m <- currentMaterial
   return $! mkTriangleMesh t m (V.fromList vs) (V.fromList fs) Nothing Nothing 
   