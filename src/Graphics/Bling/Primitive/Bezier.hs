
module Graphics.Bling.Primitive.Bezier (

   -- * Bezier Patch Meshes
   Patch, mkPatch, tesselateBezier
   
) where

import Control.Monad (forM_)
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Graphics.Bling.Math
import Graphics.Bling.Reflection
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

onePatch :: Int -> Patch -> ([Int], [(Point, Vector, Vector)])
onePatch subdivs p = (is, ps) where
   step = 1 / fromIntegral subdivs
   vstride = subdivs + 1
   is = concat [mkTris i j | i <- [0..subdivs-1], j <- [0..subdivs-1]]
   mkTris i j = [v00, v01, v10, v10, v01, v11] where
      v00 = ((i + 0) * vstride + (j + 0))
      v10 = ((i + 1) * vstride + (j + 0))
      v01 = ((i + 0) * vstride + (j + 1))
      v11 = ((i + 1) * vstride + (j + 1))
   ps = concat $ [evalv (bernstein (fromIntegral i * step)) (bernsteinDeriv (fromIntegral i * step)) | i <- [0 .. subdivs]]
   evalv bu bdu = [evalPatch p bu bdu (bernstein (fromIntegral j * step)) (bernsteinDeriv  (fromIntegral j * step)) | j <- [0 .. subdivs]]
   
tesselateBezier :: Int -> [Patch] -> Transform -> Material -> TriangleMesh
tesselateBezier subs patches t mat = mesh where
   mesh = mkTriangleMesh t mat ps is Nothing Nothing
   (ps, is) = runST $ do
      let stride = (subs+1) * (subs+1)
      iv <- MV.new (subs * subs * length patches * 3 * 2)
      pv <- MV.new (stride * length patches)
      
      forM_ (zip patches [0..]) $ \(p, pn) -> do
         let (pis, pps) = onePatch subs p
         
         forM_ (zip pis [0..]) $ \(vi, o) -> do -- indices
            MV.write iv (pn * subs * subs * 3 * 2 + o) (vi + pn * stride)
            
         forM_ (zip pps [0..]) $ \((vp, _, _), o) -> do -- points
            MV.write pv (pn * stride + o) vp
         
      pv' <- V.freeze pv
      iv' <- V.freeze iv
      return (pv', iv')
