
module Graphics.Bling.Primitive.TriangleMesh (

   TriangleMesh, mkMesh
   
   ) where

import qualified Data.Vector.Unboxed as V

import Graphics.Bling.Math
import Graphics.Bling.Transform

data TriangleMesh = Mesh
   { vIdx   :: V.Vector Int
   , ps     :: V.Vector Point
   , ns     :: Maybe (V.Vector Normal)
   , uvs    :: Maybe (V.Vector (Flt, Flt))
   }

mkMesh
   :: Transform                     -- ^ object-to-world transform
   -> V.Vector Point                -- ^ vertex positions
   -> V.Vector Int                  -- ^ vertex indices for triangles
   -> Maybe (V.Vector Normal)       -- ^ shading normals
   -> Maybe (V.Vector (Flt, Flt))   -- ^ uv coordinates for parametrization
   -> TriangleMesh
mkMesh t p i n uv = Mesh i p' n uv where
   p' = V.map (transPoint t) p
