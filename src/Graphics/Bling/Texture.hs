
module Graphics.Bling.Texture (

   -- * Texture Types
   
   Texture, SpectrumTexture, ScalarTexture,
   DiscreteTextureMap2d(..),
   DiscreteSpectrumMap2d, mkDiscreteTextureMap2d,
   ScalarMap2d, ScalarMap3d,
   
   -- * Texture Maps
   
   constSpectrumMap2d, texMap3dToTexture, texMap3dTo2d,
   
   -- * Texture Mappings
   
   TextureMapping3d, TextureMapping2d,
   identityMapping3d, uvMapping, planarMapping,
   
   -- * Textures
   
   constant, scaleTexture, addTexture, graphPaper, checkerBoard, noiseTexture,
   fbm, quasiCrystal, spectrumBlend, imageTexture, readImageTextureMap,
   readImageScalarMap,
   
   -- ** Gradients
   
   Gradient, mkGradient, gradient,
   
   -- ** Worley's Cell Noise
   
   cellNoise, euclidianDist, sqEuclidianDist, manhattanDist, chebyshevDist
   
   ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Bits
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Spectrum

--------------------------------------------------------------------------------
-- textures and texture maps
--------------------------------------------------------------------------------

type TextureMap2d a = CartesianCoords -> a
type TextureMap3d a = Point -> a

type ScalarMap2d = TextureMap2d Float
type ScalarMap3d = TextureMap3d Float

-- | A @Texture@ maps a @DifferentialGeomerty@ to some value
type Texture a = DifferentialGeometry -> a

type SpectrumTexture = Texture Spectrum
type ScalarTexture = Texture Float

type TextureMapping3d = DifferentialGeometry -> (Float, Float, Float)
type TextureMapping2d = DifferentialGeometry -> CartesianCoords

data DiscreteTextureMap2d a = TexMap
   { texMapEval   :: ! (TextureMap2d a)
   , texSize      :: {-# UNPACK #-} ! PixelSize
   }

type DiscreteSpectrumMap2d = DiscreteTextureMap2d Spectrum
type DiscreteScalarMap2d = DiscreteTextureMap2d Float

mkDiscreteTextureMap2d :: PixelSize -> TextureMap2d a -> DiscreteTextureMap2d a
mkDiscreteTextureMap2d size eval = TexMap eval size

texMap3dToTexture :: TextureMap3d a -> TextureMapping3d -> Texture a
texMap3dToTexture tm m dg = tm $ mkPoint (m dg)

texMap3dTo2d
   :: TextureMap3d a    -- ^ the 3d texture map to evaluate
   -> Float             -- ^ the fixed z value
   -> TextureMap2d a
texMap3dTo2d m z p = let (x, y) = unCartesian p in m $ mkPoint (x, y, z)

pixelSpectrum :: PixelRGB8 -> Spectrum
pixelSpectrum (PixelRGB8 r g b) = (rgbToSpectrumRefl . unGamma) (f r, f g, f b) where
   f x = fromIntegral x / 255

mod' :: Int -> Int -> Int
mod' a b = if a' < 0 then a' + b else a' where
   a' = a - n * b
   n = a `div` b

getPixel :: Image PixelRGB8 -> CartesianCoords -> Spectrum
getPixel i c =  pixelSpectrum $ pixelAt i px py where
   (u, v) = unCartesian c
   (w, h) = (imageWidth i, imageHeight i)
   px = mod' (floor $ u * fromIntegral w) w
   py = mod' (floor $ (-v) * fromIntegral h) h

getPixelScalar :: Image Pixel8 -> CartesianCoords -> Float
getPixelScalar i c = (\x -> fromIntegral x / 255) $ pixelAt i px py where
   (u, v) = unCartesian c
   (w, h) = (imageWidth i, imageHeight i)
   px = mod' (floor $ u * fromIntegral w) w
   py = mod' (floor $ (-v) * fromIntegral h) h

readImageTextureMap :: BS.ByteString -> Either String DiscreteSpectrumMap2d
readImageTextureMap bs = case decodeImage bs of
   Left err -> Left err
   Right di -> Right $ mkDiscreteTextureMap2d size eval where
      (size, eval) = case di of
         (ImageRGB8 i)     -> ((imageWidth i, imageHeight i), getPixel i)
         (ImageRGBA8 i)    -> ((imageWidth i, imageHeight i), getPixel (pixelMap dropTransparency i))
         (ImageYCbCr8 i)   -> ((imageWidth i, imageHeight i), getPixel (convertImage i))
         _ -> error "unsupported image type"

readImageScalarMap :: BS.ByteString -> Either String DiscreteScalarMap2d
readImageScalarMap bs = case decodeImage bs of
   Left err -> Left err
   Right di -> Right $ mkDiscreteTextureMap2d size eval where
      (size, eval) = case di of
         (ImageY8 i) -> ((imageWidth i, imageHeight i), getPixelScalar i)
         _ -> error "unsupported image type"

imageTexture :: DiscreteTextureMap2d a -> TextureMapping2d -> Texture a
imageTexture tm mapping dg = texMapEval tm (mapping dg)

constSpectrumMap2d :: Spectrum -> DiscreteSpectrumMap2d
constSpectrumMap2d x = TexMap (const x) (1, 1)

-- | Blends between two spectrum textures using a scalar to specify ratio.
spectrumBlend
   :: SpectrumTexture   -- ^ the first spectrum
   -> SpectrumTexture   -- ^ the second spectrum
   -> ScalarTexture     -- ^ the scalar (in [0, 1]) specifying the blending between the two spectra
   -> SpectrumTexture
spectrumBlend t1 t2 f dg
   | x <= 0 = v1
   | x >= 1 = v2
   | otherwise = sScale v1 (1 - x) + sScale v2 x
   where
      (v1, v2, x) = (t1 dg, t2 dg, f dg)

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

-- | Extracts the (u, v) parametization from the @DifferentialGeometry@ and
--   applies a scale / offset to them.
uvMapping
   :: (Float, Float)    -- ^ scale factor for (u, v)
   -> (Float, Float)    -- ^ offsets for (u, v)
   -> TextureMapping2d
uvMapping (su, sv) (ou, ov) dg = Cartesian (su * dgU dg + ou, sv * dgV dg + ov)

planarMapping
   :: (Vector, Vector)  -- ^ vectors defining the plane
   -> (Float, Float)    -- ^ offsets for (u, v)
   -> TextureMapping2d
planarMapping (vu, vv) (ou, ov) dg = Cartesian (u + ou, v + ov) where
   p = dgP dg
   u = p `dot` vu
   v = p `dot` vv
   
--------------------------------------------------------------------------------
-- Textures
--------------------------------------------------------------------------------

scaleTexture :: (Num a) => a -> a -> Texture a -> Texture a
scaleTexture a s t dg = a + s * t dg

addTexture :: (Num a) => Texture a -> Texture a -> Texture a
addTexture t1 t2 dg = t1 dg + t2 dg

graphPaper
   :: Float
   -> TextureMapping2d
   -> SpectrumTexture
   -> SpectrumTexture
   -> SpectrumTexture
graphPaper lw m p l dg
   | x' < lo || z' < lo || x' > hi || z' > hi = l dg
   | otherwise = p dg
   where
         (x, z) = unCartesian $ m dg
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

--------------------------------------------------------------------------------
-- Gradients
--------------------------------------------------------------------------------

data Gradient = Gradient
   { gradCols  :: ! (V.Vector (Float, Spectrum))
   , gradMax   :: {-# UNPACK #-} ! Float
   , gradMin   :: {-# UNPACK #-} ! Float
   } deriving (Show)

mkGradient :: [(Float, Spectrum)] -> Gradient
mkGradient ss
   | null ss = error "empty list given to mkGradient"
   | otherwise = Gradient cols (V.maximum ps) (V.minimum ps)
   where
      cols = V.fromList $ sortBy (compare `on` fst) ss
      ps = V.map fst cols

gradient :: Gradient -> ScalarTexture -> SpectrumTexture
gradient g t dg
   | f <= gradMin g = snd $ V.head $ gradCols g
   | f >= gradMax g = snd $ V.last $ gradCols g
   | otherwise = sScale c0 (1 - weight) + sScale c1 weight
   where
      f = t dg
      idx = fromJust $ V.findIndex ((> f) . fst) (gradCols g)
      e0 = V.unsafeIndex (gradCols g) (idx-1)
      e1 = V.unsafeIndex (gradCols g) idx
      (c0, c1) = (snd e0, snd e1)
      weight = (f - fst e0) / (fst e1 - fst e0)

--------------------------------------------------------------------------------
-- Worley's Cell Noise
--------------------------------------------------------------------------------

-- | a distance function takes two points and returns their distance
type DistFunc = Point -> Point -> Float

euclidianDist :: DistFunc
euclidianDist a b = len (a - b)

sqEuclidianDist :: DistFunc
sqEuclidianDist a b = sqLen (a - b)

manhattanDist :: DistFunc
manhattanDist a b = abs (vx d) + abs (vy d) + abs (vz d) where
   d = a - b

chebyshevDist :: DistFunc
chebyshevDist a b = maximum [abs (vx d), abs (vy d), abs (vz d)] where
   d = a - b

-- | Worley's cell noise, creating Voronoi patterns.
cellNoise
   :: DistFunc
   -> TextureMapping3d
   -> ScalarTexture
cellNoise dist m dg = {-# SCC "cellNoise" #-} minimum $ map (dist p) allPoints
   where
      p = mkPoint $ m dg
      
      allPoints = concatMap cellPoints ps where
         ps = [(x + ox, y + oy, z + oz) | x <- [-1..1], y <- [-1..1], z <- [-1..1]]
         (ox, oy, oz) = (floor $ vx p, floor $ vy p, floor $ vz p)
      
      cellPoints :: (Int, Int, Int) -> [Point]
      cellPoints pt@(x, y, z) = map fst $ take n $ tail $ iterate go (undefined, us) where
         us = lcg $ hash pt
         n = prob us
         go (_, u0) = (mkPoint (fromIntegral x + ox, fromIntegral y + oy, fromIntegral z + oz), u3) where
            u1 = lcg u0
            u2 = lcg u1
            u3 = lcg u2
            ox = fromIntegral u1 / 4294967296
            oy = fromIntegral u2 / 4294967296
            oz = fromIntegral u3 / 4294967296
      
      lcg :: Int -> Int
      lcg x = (1103515245 * x + 12345) `rem` 4294967296;
      
      hash :: (Int, Int, Int) -> Int
      hash (x, y, z) = abs ((x * 73856093) `xor` (y * 19349663) `xor` (z * 83492791)) `rem` 4294967296
      
      -- | a lut for getting a fast poisson distribution
      prob :: Int -> Int
      prob value
         | value < 393325350  = 1
         | value < 1022645910 = 2
         | value < 1861739990 = 3
         | value < 2700834071 = 4
         | value < 3372109335 = 5
         | value < 3819626178 = 6
         | value < 4075350088 = 7
         | value < 4203212043 = 8
         | otherwise          = 9
      
quasiCrystal
   :: Int               -- ^ number of octaves (higher adds more detail)
   -> TextureMapping2d  -- ^ the texture mapping to use
   -> ScalarTexture
quasiCrystal o t dg = {-# SCC "quasiCrystal" #-} combine (map wave (angles o)) (unCartesian $ t dg) where
   angles :: Int -> [Float]
   angles n = take n $ enumFromThen 0 (pi / fromIntegral n)
   
   combine :: [(Float, Float) -> Float] -> (Float, Float) -> Float
   combine xs = wrap . sum . sequence xs where
      wrap n = case aux n of
         (k, v) | odd (k::Int) -> 1 - v
                | otherwise    -> v

      aux n = case properFraction n of
         kv@(k, v) | v < 0     -> (k - 1, 1 + v)
                   | otherwise -> kv
   
   wave :: Float -> (Float, Float) -> Float
   wave th = f where
      (cth, sth) = (cos th, sin th)
      f (x, y) = (cos (cth * x + sth * y) + 1) / 2


-- | Fractal Brownian Motion (FBM)
fbm
   :: Int               -- ^ number of octaves (higher -> more detail)
   -> Float             -- ^ lambda (roughness)
   -> ScalarMap3d
fbm octaves omega p = sum $ take octaves go where
   go = map (\(l, o) -> o * perlin3d (px * l, py * l, pz * l)) $ zip ls os
   ls = iterate (1.99 *) 1
   os = iterate (omega *) 1
   (px, py, pz) = (vx p, vy p, vz p)
   
-- | A simple Perlin noise in 3D using the specified texture mapping.
noiseTexture
   :: TextureMapping3d  -- ^ the texture mapping to use
   -> ScalarTexture     -- ^ the resulting texture
noiseTexture texMap dg = perlin3d $ texMap dg

perlin3d :: (Float, Float, Float) -> Float
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
   
noiseWeight :: Float -> Float
noiseWeight t = 6 * t4 * t - 15 * t4 + 10 * t3 where
   t3 = t * t * t
   t4 = t3 * t
   
grad :: Int -> Int -> Int -> Float -> Float -> Float -> Float
grad x y z dx dy dz = u + v where
   u = if (h .&. 1) /= 0 then (-u') else u'
   v = if h .&. 2 /= 0 then (-v') else v'
   u' = if h < 8 || h `elem` [12, 13] then dx else dy
   v' = if h < 4 || h `elem` [12, 13] then dy else dz
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
