
module Graphics.Bling.Texture (
   -- * Texture Types
   
   Texture, SpectrumTexture, ScalarTexture, TextureMapping3d,
   TextureMap(..), SpectrumMap, constSpectrumMap, mkTextureMap,
   
   -- * Texture Mappings

   identityMapping3d,
   
   -- * Creating Textures
   constant, scaleTexture, graphPaper, checkerBoard, noiseTexture, woodTexture
   ) where

import Data.Bits
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Spectrum

--------------------------------------------------------------------------------
-- textures and texture maps
--------------------------------------------------------------------------------

-- | A @Texture@ transforms a @DifferentialGeomerty@ to some value
type Texture a = DifferentialGeometry -> a

type SpectrumTexture = Texture Spectrum
type ScalarTexture = Texture Flt

type TextureMapping3d = DifferentialGeometry -> (Flt, Flt, Flt)

data TextureMap a = TexMap
   { texMapEval   :: CartesianCoords -> a
   , texSize      :: !PixelSize
   }

mkTextureMap :: PixelSize -> (CartesianCoords -> a) -> TextureMap a
mkTextureMap size eval = TexMap eval size

type SpectrumMap = TextureMap Spectrum

constSpectrumMap :: Spectrum -> SpectrumMap
constSpectrumMap x = TexMap (const x) (1, 1)

--------------------------------------------------------------------------------
-- Texture Mappings
--------------------------------------------------------------------------------

-- | maps world-space points to texture space using a @Transform@
identityMapping3d
   :: Transform -- ^ the world-to-texture transformation
   -> TextureMapping3d
identityMapping3d w2t dg = (x, y, z) where
   (Vector x y z) = transPoint w2t $ dgP dg

-- | a constant texture, always returning the same value
constant
   :: a -- the value of the texture
   -> Texture a
constant r _ = r

--------------------------------------------------------------------------------
-- Textures
--------------------------------------------------------------------------------

scaleTexture :: (Num a) => a -> Texture a -> Texture a
scaleTexture s t dg = s * (t dg)

graphPaper :: Flt -> SpectrumTexture -> SpectrumTexture -> SpectrumTexture
graphPaper lw p l dg
   | x' < lo || z' < lo || x' > hi || z' > hi = l dg
   | otherwise = p dg
   where
         (x, z) = (vx $ dgP dg, vz $ dgP dg)
         x' = abs x''
         z' = abs z''
         (_, x'') = properFraction x :: (Int, Float)
         (_, z'') = properFraction z :: (Int, Float)
         lo = lw / 2
         hi = 1.0 - lo

checkerBoard
   :: Vector -- ^ scale
   -> Texture a -- ^ first texture
   -> Texture a -- ^ second texture
   -> Texture a
   
checkerBoard (Vector sx sy sz) t1 t2 dg
   | (floor (x * sx) + floor (y * sy) + floor (z * sz) :: Int) `mod` 2 == 0 = t1 dg
   | otherwise = t2 dg
   where
      (Vector x y z) = dgP dg
--
-- Wood
--

woodTexture :: SpectrumTexture
woodTexture dg = wood where
   g = perlin3d (x * 4, y * 4, z * 4)
   grain = abs $ g - fromIntegral (floor g :: Int)
   wood = fromRGB (grain, grain, 0.1)
   (Vector x y z) = dgP dg
   
--
-- Perlin Noise
--

noiseTexture :: TextureMapping3d -> ScalarTexture
noiseTexture texMap dg = perlin3d $ texMap dg

perlin3d :: (Flt, Flt, Flt) -> Flt
perlin3d (x, y, z) = lerp wz y0 y1 where
   -- cell coordinates and offsets
   (ix', iy', iz') = (floor x, floor y, floor z) :: (Int, Int, Int)
   (dx, dy, dz) = (x - fromIntegral ix', y - fromIntegral iy', z - fromIntegral iz')
   (ix, iy, iz) = (ix' .&. 255, iy' .&. 255, iz' .&. 255)
   
   -- gradient weights
   w000 = grad  ix     iy     iz     dx     dy     dz
   w100 = grad (ix+1)  iy     iz    (dx-1)  dy     dz
   w010 = grad  ix    (iy+1)  iz     dx    (dy-1)  dz
   w110 = grad (ix+1) (iy+1)  iz    (dx-1) (dy-1)  dz
   w001 = grad  ix     iy    (iz+1)  dx     dy    (dz-1)
   w101 = grad (ix+1)  iy    (iz+1) (dx-1)  dy    (dz-1)
   w011 = grad  ix    (iy+1) (iz+1)  dx    (dy-1) (dz-1)
   w111 = grad (ix+1) (iy+1) (iz+1) (dx-1) (dy-1) (dz-1)
   
   -- trilinear weight interpolation
   y0 = lerp wy x00 x10
   y1 = lerp wy x01 x11
   x00 = lerp wx w000 w100
   x10 = lerp wx w010 w110
   x01 = lerp wx w001 w101
   x11 = lerp wx w011 w111
   wx = noiseWeight dx
   wy = noiseWeight dy
   wz = noiseWeight dz
   
noiseWeight :: Flt -> Flt
noiseWeight t = 6 * t4 * t - 15 * t4 + 10 * t3 where
   t3 = t * t * t
   t4 = t3 * t
   
grad :: Int -> Int -> Int -> Flt -> Flt -> Flt -> Flt
grad x y z dx dy dz = u + v where
   u = if (h .&. 1) /= 0 then (-u') else u'
   v = if h .&. 2 /= 0 then (-v') else v'
   u' = if h < 8 || h == 12 || h == 13 then dx else dy
   v' = if h < 4 || h == 12 || h == 13 then dy else dz
   h = h' .&. 15
   h' = noisePerms V.! ((noisePerms V.! ((noisePerms V.! x) + y)) + z)
   
noisePerms :: V.Vector Int
noisePerms = V.fromList (l ++ l) where
  l = [ 151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180 ]
