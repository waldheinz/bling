
module Graphics.Bling.Primitive.Heightmap (
   
   heightMap

   ) where

import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.TriangleMesh
import Graphics.Bling.Reflection
import Graphics.Bling.Texture

import qualified Data.Vector.Unboxed as UV

heightMap
   :: ScalarMap2d
   -> PixelSize
   -> Material
   -> Transform
   -> Primitive
   
heightMap elev (ns, nt) mat t = mkTriangleMesh t mat (UV.fromList ps) is Nothing Nothing where
   ps = [let (x', z') = (fromIntegral x / fromIntegral (ns-1), fromIntegral z / fromIntegral (nt-1)) in mkPoint (x', elev $ Cartesian (x', z'), z') | z <- [0..nt-1], x <- [0..ns-1]]
   
   is = UV.fromList $ concatMap tris [(x, y) | y <- [0..(nt-2)], x <- [0..(ns-2)]]
   
   tris pos = fstTri pos ++ sndTri pos
   fstTri (x, y) = [vert (x, y), vert (x+1, y), vert (x+1, y+1)]
   sndTri (x, y) = [vert (x, y), vert (x+1, y+1), vert (x, y+1)]
   vert (x, y) = x + y * ns
   
