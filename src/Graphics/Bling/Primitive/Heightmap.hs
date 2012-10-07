
module Graphics.Bling.Primitive.Heightmap (
   
   heightMap

   ) where

import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.TriangleMesh
import Graphics.Bling.Reflection
import Graphics.Bling.Texture
import Graphics.Bling.Utils

import qualified Data.Vector.Unboxed as UV

heightMap
   :: ScalarMap2d
   -> PixelSize
   -> Material
   -> Transform
   -> Primitive
   
heightMap elev (ns, nt) mat t = mkTriangleMesh t mat ps is norms uvs where   
   (fns, fnt) = (fromIntegral ns, fromIntegral nt)
   
   -- triangle indices
   is = UV.fromList $ concatMap tris [(x, y) | y <- [0..(nt-2)], x <- [0..(ns-2)]]
   tris pos = fstTri pos ++ sndTri pos
   fstTri (x, y) = [vert (x, y), vert (x+1, y), vert (x+1, y+1)]
   sndTri (x, y) = [vert (x, y), vert (x+1, y+1), vert (x, y+1)]
   vert (x, y) = x + y * ns
   
   -- coordinates
   coords = [(fromIntegral x / (fns - 1), fromIntegral z / (fnt - 1))
                  | z <- [0..nt-1], x <- [0..ns-1]]
   
   ps = UV.fromList $ map pt coords
   pt (x, z) = mkPoint (x, elev $ Cartesian (x, z), z)
   
   -- normals
   (ex, ez) = (1 / fns, 1 / fnt) -- epsilon in x and z
   
   norms = Just $ UV.fromList $ map norm coords
   norm (x, z) = - (normalize $ mkV (dx, ex + ez, dz)) where
      dx = elev (Cartesian (x - ex, z)) - elev (Cartesian (x + ex, z))
      dz = elev (Cartesian (x, z - ez)) - elev (Cartesian (x, z + ez))
      
   -- uv
   uvs = Just $ flatTuple $ UV.fromList $ map uv coords
   uv (x, z) = (x / (fns - 1), z / (fns - 1))
   
