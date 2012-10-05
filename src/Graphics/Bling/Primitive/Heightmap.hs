
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
   
heightMap elev (ns, nt) mat t = mkTriangleMesh t mat ps is ns' Nothing where
   
   -- triangle indices
   is = UV.fromList $ concatMap tris [(x, y) | y <- [0..(nt-2)], x <- [0..(ns-2)]]
   tris pos = fstTri pos ++ sndTri pos
   fstTri (x, y) = [vert (x, y), vert (x+1, y), vert (x+1, y+1)]
   sndTri (x, y) = [vert (x, y), vert (x+1, y+1), vert (x, y+1)]
   vert (x, y) = x + y * ns
   
   -- vertex coordinates
   coords = [
      (fromIntegral x / fromIntegral (ns-1),
       fromIntegral z / fromIntegral (nt-1))
       
          | z <- [0..nt-1], x <- [0..ns-1]]
       
   ps = UV.fromList $ map pt coords
   pt (x, z) = mkPoint (x, elev $ Cartesian (x, z), z)
   
   -- vertex normals
   ex = 1 / fromIntegral ns
   ez = 1 / fromIntegral nt
   
   ns' = Nothing -- Just norms
   norms = UV.fromList $ map norm coords
   norm (x, z) = - (normalize $ mkV (dx, 3 * (ex + ez), dz)) where
      dx = (elev $ Cartesian (x - ex, z)) - (elev $ Cartesian (x + ex, z))
      dz = (elev $ Cartesian (x, z + ez)) - (elev $ Cartesian (x, z + ez))
      
