
module Graphics.Bling.Primitive.Bezier (

   -- * Bezier Patch Meshes
   Patch, mkPatch, tesselateBezierMesh
) where

import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.Math
import Graphics.Bling.Shape
import Graphics.Bling.Primitive.TriangleMesh

newtype Patch = Patch { _unPatch :: V.Vector Flt }

mkPatch :: [Flt] -> Either String Patch
mkPatch xs
   | length xs == 48 = Right $ Patch $ V.fromList xs
   | otherwise = Left "must give 48 values per patch"

type Bernstein = Int -> Flt

bernstein :: Flt -> Bernstein
bernstein u x
   | x == 0 = 1 * i * i * i
   | x == 1 = 3 * u * i * i
   | x == 2 = 3 * u * u * i
   | x == 3 = 1 * u * u * u
   | otherwise = error $ "bernstein for " ++ (show x)
   where
      i = 1 - u

type BernsteinDeriv = Int -> Flt

bernsteinDeriv :: Flt -> BernsteinDeriv
bernsteinDeriv u x
   | x == 0 = 3 * (0 - i * i)
   | x == 1 = 3 * (i * i - 2 * u * i)
   | x == 2 = 3 * (2 * u * i - u * u)
   | x == 3 = 3 * (u * u - 0)
   | otherwise = error $ "bernsteinDeriv for " ++ (show x)
   where
      i = 1 - u

-- | evaluates a patch using the specified Bernstein factors
evalPatch
   :: Patch -- ^ the patch to evaluate
   -> Bernstein -- ^ bernstein in u
   -> BernsteinDeriv -- ^ bernstein derivative in u
   -> Bernstein -- ^ bernstein in v
   -> BernsteinDeriv -- ^ bernstein derivative in v
   -> (Point, Vector, Vector) -- ^ (p, dpdu, dpdv)
evalPatch (Patch ctrl) bu bdu bv bdv = (p, dpdu, dpdv) where
   p = ev bu bv
   dpdu = ev bdu bv
   dpdv = ev bu bdv
   c = (V.!) ctrl -- V.unsafeIndex ctrl
   ev bj bi = mkV (s 0, s 1, s 2) where
      s o = sum [c (i * 12 + j * 3 + o) * bj j * bi i | i <- [0..3], j <- [0..3]]

tesselatePatchFlat :: Int -> Patch -> [Shape]
tesselatePatchFlat subdivs p = ts where
   step = 1 / fromIntegral subdivs
   vstride = subdivs + 1
   ts = concat [mkTris i j | i <- [0..subdivs-1], j <- [0..subdivs-1]]
   mkTris i j = [mkTriangle (v00, v01, v10), mkTriangle (v10, v01, v11)] where
      v00 = vs VB.! ((i + 0) * vstride + (j + 0))
      v10 = vs VB.! ((i + 1) * vstride + (j + 0))
      v01 = vs VB.! ((i + 0) * vstride + (j + 1))
      v11 = vs VB.! ((i + 1) * vstride + (j + 1))
   vs = VB.fromList $ map (\(pt, _, _) -> Vertex pt) ps
   ps = concat $ [evalv (bernstein (fromIntegral i * step)) (bernsteinDeriv (fromIntegral i * step)) | i <- [0 .. subdivs]]
   evalv bu bdu = [evalPatch p bu bdu (bernstein (fromIntegral j * step)) (bernsteinDeriv  (fromIntegral j * step)) | j <- [0 .. subdivs]]
   
tesselateBezierMesh :: Int -> [Patch] -> [Shape]
tesselateBezierMesh subdivs ps = concatMap (tesselatePatchFlat subdivs) ps
