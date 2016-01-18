{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module Graphics.Bling.Spectrum (

   Spectrum, WeightedSpectrum(..), ImageSample, Contribution,
   white, black,

   -- * Spectrum conversions
   rgbToSpectrumRefl, rgbToSpectrumIllum, unGamma,

   -- * Working with SPDs
   Spd, mkSpd, mkSpd', mkSpdFunc, fromCIExy, spdToXYZ, evalSpd,

   isBlack, sNaN, sInfinite,
   xyzToRgb,  toRGB, fromSpd, sConst, sBlackBody, sY,
   sScale, sPow, sClamp, sClamp', chromaticityToXYZ,
   spectrumToXYZ, xyzToSpectrum

   ) where

import Control.Monad (liftM, forM_)
import Data.List (sortBy)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import Control.DeepSeq as DS
import Prelude as P

import Graphics.Bling.Math

-- | the number of spectral bands we use for a spectrum
bands :: Int
{-# INLINE bands #-}
bands = 16

spectrumLambdaStart :: Float
{-# INLINE spectrumLambdaStart #-}
spectrumLambdaStart = 400

spectrumLambdaEnd :: Float
{-# INLINE spectrumLambdaEnd #-}
spectrumLambdaEnd = 700

newtype Spectrum = Spectrum { unSpectrum :: V.Vector Float } deriving (Show)

instance DS.NFData Spectrum where
   rnf (Spectrum v) = seq v ()
   {-# INLINE rnf #-}

--------------------------------------------------------------------------------
-- Unboxed Vectors of Spectra
--------------------------------------------------------------------------------

newtype instance V.MVector s Spectrum = MV_Spectrum (V.MVector s Float)
newtype instance V.Vector Spectrum = V_Spectrum (V.Vector Float)

instance V.Unbox Spectrum

instance MV.MVector V.MVector Spectrum where
   basicLength (MV_Spectrum v) = MV.basicLength v `div` bands
   {-# INLINE basicLength #-}

   basicUnsafeSlice s l (MV_Spectrum v) =
      MV_Spectrum (MV.unsafeSlice (s * bands) (l * bands) v)
   {-# INLINE basicUnsafeSlice #-}

   basicUnsafeNew l = MV_Spectrum `liftM` MV.unsafeNew (l * bands)
   {-# INLINE basicUnsafeNew #-}

   basicInitialize _ = return ()

   basicOverlaps (MV_Spectrum v1) (MV_Spectrum v2) = MV.overlaps v1 v2
   {-# INLINE basicOverlaps #-}

   basicUnsafeRead (MV_Spectrum v) idx =
      V.generateM bands (\i -> MV.unsafeRead v $(idx * bands) + i)
      >>= \v' -> return (Spectrum v')
   {-# INLINE basicUnsafeRead #-}

   basicUnsafeWrite (MV_Spectrum v) idx (Spectrum vs) =
      forM_ [0..bands-1] $ \i -> MV.unsafeWrite v ((idx * bands) + i) (V.unsafeIndex vs i)
   {-# INLINE basicUnsafeWrite #-}

instance GV.Vector V.Vector Spectrum where
   basicLength (V_Spectrum v) = GV.basicLength v `div` bands
   {-# INLINE basicLength #-}

   basicUnsafeSlice s l (V_Spectrum v) =
      V_Spectrum $ (GV.unsafeSlice (s * bands) (l * bands) v)
   {-# INLINE basicUnsafeSlice #-}

   basicUnsafeFreeze (MV_Spectrum v) = V_Spectrum `liftM` (GV.unsafeFreeze v)
   {-# INLINE basicUnsafeFreeze #-}

   basicUnsafeThaw (V_Spectrum v) = MV_Spectrum `liftM` (GV.unsafeThaw v)
   {-# INLINE basicUnsafeThaw #-}

   basicUnsafeIndexM (V_Spectrum v) idx =
      V.generateM bands (\i -> GV.unsafeIndexM v ((idx * bands) + i))
      >>= \v' -> return (Spectrum v')
   {-# INLINE basicUnsafeIndexM #-}

-- | a "black" @Spectrum@ (no transmittance or emission at all wavelengths)
black :: Spectrum
{-# INLINE black #-}
black = sConst 0

-- | a "white" @Spectrum@ (unit transmission at all wavelengths)
white :: Spectrum
{-# INLINE white #-}
white = sConst 1

-- | Removes the gamma correction from a RGB triple. The supplied RGB value is
--   supposed to have a gamma correction of 2.2 applied, which is reversed
--   by this function.
unGamma :: (Float, Float, Float) -> (Float, Float, Float)
{-# INLINE unGamma #-}
unGamma (r, g, b) = let ga = 2.2 in (r ** ga, g ** ga, b ** ga)

data RGBToSpectrumBase =
   RGBBases
      !Spectrum !Spectrum !Spectrum -- r, g, b
      !Spectrum !Spectrum !Spectrum -- c, m, y
      !Spectrum                     -- w

rgbReflectance :: RGBToSpectrumBase
rgbReflectance = RGBBases
   rgbReflRed  rgbReflGreen   rgbReflBlue
   rgbReflCyan rgbReflMagenta rgbReflYellow
   rgbReflWhite

rgbIlluminant :: RGBToSpectrumBase
rgbIlluminant = RGBBases
   rgbIllumRed  rgbIllumGreen   rgbIllumBlue
   rgbIllumCyan rgbIllumMagenta rgbIllumYellow
   rgbIllumWhite

rgbToSpectrumRefl :: (Float, Float, Float) -> Spectrum
rgbToSpectrumRefl = rgbToSpectrum rgbReflectance

rgbToSpectrumIllum :: (Float, Float, Float) -> Spectrum
rgbToSpectrumIllum = rgbToSpectrum rgbIlluminant

rgbToSpectrum :: RGBToSpectrumBase -> (Float, Float, Float) -> Spectrum
rgbToSpectrum (RGBBases rb gb bb cb mb yb wb) (r, g, b)
   | r <= g && r <= b =
      sScale wb r + if g <= b
         then sScale cb (g - r) + sScale bb (b - g)
         else sScale cb (b - r) + sScale gb (g - b)
   |  g <= r && g <= b =
      sScale wb g + if r <= b
         then sScale mb (r - g) + sScale bb (b - r)
         else sScale mb (b - g) + sScale rb (r - b)
   | otherwise =
      sScale wb b + if r <= b
         then sScale yb (r - b) + sScale gb (g - r)
         else sScale yb (g - b) + sScale rb (r - g)

-- | converts from CIE XYZ to sRGB
xyzToRgb
   :: (Float, Float, Float) -- ^ (X, Y, Z)
   -> (Float, Float, Float) -- ^ (r, g, b)
xyzToRgb (x, y, z) = (r, g, b) where
   r =   3.240479  * x - 1.537150 * y - 0.498535 * z
   g = (-0.969256) * x + 1.875991 * y + 0.041556 * z
   b =   0.055648  * x - 0.204043 * y + 1.057311 * z

--------------------------------------------------------------------------------
-- ImageSample and the like
--------------------------------------------------------------------------------

data WeightedSpectrum = WS {-# UNPACK #-} !Float !Spectrum -- the sample weight and the sampled spectrum
type ImageSample = (Float, Float, WeightedSpectrum) -- the pixel coordinates and the weighted spectrum
type Contribution = (Bool, ImageSample)


--------------------------------------------------------------------------------
-- SPDs
--------------------------------------------------------------------------------

data Spd
   = IrregularSpd
      { _spdLambdas :: !(V.Vector Float)
      ,  _spdValues :: !(V.Vector Float) }
   | RegularSpd
      {-# UNPACK #-} !Float   -- min lambda
      {-# UNPACK #-} !Float   -- max lambda
      !(V.Vector Float)       -- amplitudes
   | Chromaticity
      {-# UNPACK #-} !Float   -- M1
      {-# UNPACK #-} !Float   -- M2
   | SpdFunc
      !(Float -> Float)       -- defined by a function

-- | creates a SPD from a list of (lambda, value) pairs, which must
--   not be empty
mkSpd
   :: [(Float, Float)] -- ^ the SPD as (lambda, amplitude) pairs
   -> Spd
mkSpd [] = error "empty SPD"
mkSpd xs = IrregularSpd ls vs where
   ls = V.fromList (P.map fst sorted)
   vs = V.fromList (P.map snd sorted)
   sorted = sortBy cmp xs
   cmp (l1, _) (l2, _) = compare l1 l2

-- | creates a SPD from a list of regulary sampled amplitudes
mkSpd'
   :: Float    -- ^ the wavelength of the first amplitude sample
   -> Float    -- ^ the wavelength of the last amplitude sample
   -> [Float]  -- ^ the amplitudes of the SPD, must not be empty
   -> Spd      -- ^ the resulting SPD
mkSpd' s e vs = RegularSpd s e (V.fromList vs)

mkSpdFunc
   :: (Float -> Float) -- ^ the SPD function from lambda in nanometers to amplitude
   -> Spd
mkSpdFunc = SpdFunc

fromCIExy
   :: Float
   -> Float
   -> Spd
fromCIExy x y = Chromaticity m1 m2 where
   (m1, m2) = chromaParams x y

chromaParams :: Float -> Float -> (Float, Float)
chromaParams x y = (m1, m2) where
   m1 = (-1.3515 - 1.7703 * x + 5.9114 * y) / (0.0241 + 0.2562 * x - 0.7341 * y)
   m2 = (0.03 - 31.4424 * x + 30.0717 * y) / (0.0241 + 0.2562 * x - 0.7341 * y)

s0XYZ :: (Float, Float, Float)
s0XYZ = spdToXYZ cieS0

s1XYZ :: (Float, Float, Float)
s1XYZ = spdToXYZ cieS1

s2XYZ :: (Float, Float, Float)
s2XYZ = spdToXYZ cieS2

chromaticityToXYZ :: Float -> Float -> (Float, Float, Float)
chromaticityToXYZ x y = (x', y', z') where
   (s0x, s0y, s0z) = s0XYZ
   (s1x, s1y, s1z) = s1XYZ
   (s2x, s2y, s2z) = s2XYZ
   (m1, m2) = chromaParams x y
   x' = s0x + m1 * s1x + m2 * s2x
   y' = s0y + m1 * s1y + m2 * s2y
   z' = s0z + m1 * s1z + m2 * s2z

-- | evaluates a SPD at a given wavelength
evalSpd
   :: Spd -- ^ the SPD to evaluate
   -> Float -- ^ the lambda where the SPD should be evaluated
   -> Float -- ^ the SPD value at the specified lambda
evalSpd (IrregularSpd ls vs) l
   | l <= V.head ls = V.head vs
   | l >= V.last ls = V.last vs
   | otherwise = lerp t (vs V.! i) (vs V.! (i+1)) where
      t = (l - (ls V.! i)) / ((ls V.! (i+1)) - (ls V.! i))
      i = fi 0 (V.length ls - 1)
      fi lo hi -- binary search for index
         | lo == mid = lo
         | (ls V.! mid) == l = mid
         | (ls V.! mid) < l = fi mid hi
         | otherwise = fi lo mid where
            mid = (lo + hi) `div` 2

evalSpd (RegularSpd l0 l1 amps) l
   | l <= l0 = V.head amps
   | l >= l1 = V.last amps
   | otherwise = (1 - dx) * (amps V.! b0) + dx * (amps V.! b1)
   where
      d1 = 1 / ((l1 - l0) / fromIntegral (V.length amps - 1)) -- 1 / delta
      x = (l - l0) * d1
      b0 = floor x
      b1 = min (b0 + 1) (V.length amps - 1)
      dx = x - fromIntegral b0

evalSpd (Chromaticity m1 m2) l = s0 + m1 * s1 + m2 * s2 where
   s0 = evalSpd cieS0 l
   s1 = evalSpd cieS1 l
   s2 = evalSpd cieS2 l

evalSpd (SpdFunc f) l = f l

-- | determines the average value of a @Spd@ in the specified interval
--   TODO: should compute the weighted average
avgSpd
   :: Spd   -- ^ the Spd to evaluate
   -> Float -- ^ the minimum wavelength of interest
   -> Float -- ^ the maximum wavelength of interest, must be >= the minimum
   -> Float

avgSpd (RegularSpd s0 s1 amps) l0 l1
   | l1 <= s0 = V.head amps
   | l0 >= s1 = V.last amps
   | otherwise = V.sum amps' / fromIntegral (V.length amps')
   where
      amps' = V.slice i0 (i1 - i0 + 1) amps
      n = V.length amps
      i0 = max 0 $ min n $ floor $ fromIntegral n * ((l0 - s0) / (s1 - s0))
      i1 = max 0 $ min n $ floor $ fromIntegral n * ((l1 - s0) / (s1 - s0))

avgSpd (IrregularSpd ls vs) l0 l1
   | l1 <= V.head ls = V.head vs
   | l0 >= V.last ls = V.last vs
   | otherwise = V.sum vs' / fromIntegral (V.length vs')
   where
      vs' = V.slice i0 (i1 - i0 + 1) vs
      i0 = maybe 0 id $ V.findIndex (>= l0) ls
      i1 = maybe (V.length vs - 1) id $ V.findIndex (>= l1) ls

avgSpd spd l0 l1 = (evalSpd spd l0 + evalSpd spd l1) * 0.5

-- | converts a @Spd@ to CIE XYZ values
spdToXYZ :: Spd -> (Float, Float, Float)
spdToXYZ spd = (x / yint, y / yint, z / yint) where
   ls = [cieStart .. cieEnd]
   vs = P.map (\l -> evalSpd spd (fromIntegral l)) ls
   yint = P.sum (P.map (\l -> evalSpd cieY (fromIntegral l)) ls)
   x = P.sum $ P.zipWith (*) (P.map (\l -> evalSpd cieX (fromIntegral l)) ls) vs
   y = P.sum $ P.zipWith (*) (P.map (\l -> evalSpd cieY (fromIntegral l)) ls) vs
   z = P.sum $ P.zipWith (*) (P.map (\l -> evalSpd cieZ (fromIntegral l)) ls) vs

-- | converts from a @Spd@ to @Spectrum@
fromSpd
   :: Spd
   -> Spectrum
fromSpd spd = Spectrum $ V.generate bands go where
   go i = avgSpd spd l0 l1 where
      l0 = lerp (fromIntegral i / fromIntegral bands) spectrumLambdaStart spectrumLambdaEnd
      l1 = lerp (fromIntegral (i+1) / fromIntegral bands) spectrumLambdaStart spectrumLambdaEnd

spectrumCieX :: Spectrum
spectrumCieX = fromSpd cieX

spectrumCieY :: Spectrum
spectrumCieY = fromSpd cieY

spectrumCieZ :: Spectrum
spectrumCieZ = fromSpd cieZ

spectrumCieYSum :: Float
spectrumCieYSum = V.sum $ unSpectrum spectrumCieY

spectrumToXYZ :: Spectrum -> (Float, Float, Float)
spectrumToXYZ s = scale $ V.foldl' (\(a, b, c) (v, x, y, z) -> (a + x * v, b + y * v, c + z * v)) (0, 0, 0) $ V.zip4 vsv vsx vsy vsz where
   vsx = unSpectrum spectrumCieX
   vsy = unSpectrum spectrumCieY
   vsz = unSpectrum spectrumCieZ
   vsv = unSpectrum s
   scale (x, y, z) = (x / spectrumCieYSum, y / spectrumCieYSum, z / spectrumCieYSum)

xyzToSpectrum :: (Float, Float, Float) -> Spectrum
xyzToSpectrum = rgbToSpectrumIllum . xyzToRgb
{-
xyzToSpectrum (x, y, z) =
   sScale spectrumCieX (x / spectrumCieYSum) +
   sScale spectrumCieY (y / spectrumCieYSum) +
   sScale spectrumCieZ (z / spectrumCieYSum)
  -}

toRGB :: Spectrum -> (Float, Float, Float)
{-# INLINE toRGB #-}
toRGB (Spectrum v) = (v V.! 0, v V.! 1,  v V.! 2)

-- | the brightness
sY :: Spectrum -> Float
{-# INLINE sY #-}
sY (Spectrum v) = (V.sum $ V.zipWith (*) v $ unSpectrum spectrumCieY) / spectrumCieYSum

sConst :: Float -> Spectrum
{-# INLINE sConst #-}
sConst r = Spectrum $ V.replicate bands r

sMap :: (Float -> Float) -> Spectrum -> Spectrum
{-# INLINE sMap #-}
sMap f s = Spectrum $ V.map f $ unSpectrum s

instance Floating Spectrum where
   pi    = sConst pi
   {-# INLINE pi #-}
   exp   = sMap exp
   {-# INLINE exp #-}
   sqrt  = sMap sqrt
   {-# INLINE sqrt #-}
   log   = sMap log
   {-# INLINE log #-}
   (**) (Spectrum s1) (Spectrum s2) = Spectrum $ V.zipWith (**) s1 s2
   {-# INLINE (**) #-}
   logBase (Spectrum s1) (Spectrum s2) = Spectrum $ V.zipWith logBase s1 s2
   {-# INLINE logBase #-}
   sin   = sMap sin
   {-# INLINE sin #-}
   tan   = sMap tan
   {-# INLINE tan #-}
   cos   = sMap cos
   {-# INLINE cos #-}
   asin  = sMap asin
   {-# INLINE asin #-}
   atan  = sMap atan
   {-# INLINE atan #-}
   acos  = sMap acos
   {-# INLINE acos #-}
   sinh  = sMap sinh
   {-# INLINE sinh #-}
   tanh  = sMap tanh
   {-# INLINE tanh #-}
   cosh  = sMap cosh
   {-# INLINE cosh #-}
   asinh = sMap asinh
   {-# INLINE asinh #-}
   atanh = sMap atanh
   {-# INLINE atanh #-}
   acosh = sMap acosh
   {-# INLINE acosh #-}

instance Fractional Spectrum where
   Spectrum v1 / Spectrum v2 = Spectrum $ V.zipWith (/) v1 v2
   {-# INLINE (/) #-}
   fromRational i = Spectrum $ V.replicate bands (fromRational i)
   {-# INLINE fromRational #-}

instance Num Spectrum where
   Spectrum v1 + Spectrum v2 = Spectrum $ V.zipWith (+) v1 v2
   {-# INLINE (+) #-}
   Spectrum v1 - Spectrum v2 = Spectrum $ V.zipWith (-) v1 v2
   {-# INLINE (-) #-}
   Spectrum v1 * Spectrum v2 = Spectrum $ V.zipWith (*) v1 v2
   {-# INLINE (*) #-}
   abs (Spectrum v) = Spectrum $ V.map abs v
   {-# INLINE abs #-}
   negate (Spectrum v) = Spectrum $ V.map negate v
   {-# INLINE negate #-}
   signum (Spectrum v) = Spectrum $ V.map signum v
   {-# INLINE signum #-}
   fromInteger i = Spectrum $ V.replicate bands (fromInteger i)
   {-# INLINE fromInteger #-}

-- | Decides if a @Spectrum@ is black
isBlack :: Spectrum -> Bool
{-# INLINE isBlack #-}
isBlack (Spectrum v) = V.all (== 0) v

sScale :: Spectrum -> Float -> Spectrum
{-# INLINE sScale #-}
sScale (Spectrum v) f = Spectrum $ V.map (*f) v

-- | clamps the @Spectrum@ coefficients the specified range
sClamp :: Float -> Float -> Spectrum -> Spectrum
{-# INLINE sClamp #-}
sClamp smin smax (Spectrum v) = Spectrum $ V.map c v where
   c x = max smin $ min smax x

-- | clamps the @Spectrum@ coefficients to [0,1]
sClamp' :: Spectrum -> Spectrum
{-# INLINE sClamp' #-}
sClamp' = sClamp 0 1

sNaN :: Spectrum -> Bool
{-# INLINE sNaN #-}
sNaN (Spectrum v) = V.any isNaN v

sInfinite :: Spectrum -> Bool
{-# INLINE sInfinite #-}
sInfinite (Spectrum v) = V.any isInfinite v

sPow :: Spectrum -> Spectrum -> Spectrum
{-# INLINE sPow #-}
sPow (Spectrum vc) (Spectrum ve) = Spectrum (V.zipWith p' vc ve) where
   p' :: Float -> Float -> Float
   p' c e
      | c > 0 = c ** e
      | otherwise = 0

-- | The spectrum of a black body emitter
sBlackBody
   :: Float -- ^ the temperature in Kelvin
   -> Spectrum -- ^ the emission spectrum of the emitter
sBlackBody t = fromSpd $ mkSpdFunc (planck t)

--sScale (fromXYZ (x, y, z)) (1 / (fromIntegral (cieEnd - cieStart))) where
--   z = max 0 $ P.sum $ P.map (\wl -> (cieZ wl) * (p wl)) [cieStart .. cieEnd]
--   y = max 0 $ P.sum $ P.map (\wl -> (cieY wl) * (p wl)) [cieStart .. cieEnd]
--   x = max 0 $ P.sum $ P.map (\wl -> (cieX wl) * (p wl)) [cieStart .. cieEnd]
--   p = (\wl -> planck t (fromIntegral wl))

planck :: RealFloat a => a -> a -> a
planck temp w = (0.4e-9 * (3.74183e-16 * w' ^^ (-5::Int))) / (exp (1.4388e-2 / (w' * temp)) - 1) where
   w' = w * 1e-9

--
-- CIE chromaticity SPDs
--

cieS0 :: Spd
cieS0 = mkSpd' 300 830
   [  0.04,   6.0,  29.6,  55.3,  57.3,  61.8,  61.5,  68.8,  63.4,
      65.8,  94.8, 104.8, 105.9,  96.8, 113.9, 125.6, 125.5, 121.3,
     121.3, 113.5, 113.1, 110.8, 106.5, 108.8, 105.3, 104.4, 100.0,
      96.0,  95.1,  89.1,  90.5,  90.3,  88.4,  84.0,  85.1,  81.9,
      82.6,  84.9,  81.3,  71.9,  74.3,  76.4,  63.3,  71.7,  77.0,
      65.2,  47.7,  68.6,  65.0,  66.0,  61.0,  53.3,  58.9,  61.9 ]

cieS1 :: Spd
cieS1 = mkSpd' 300 830
   [ 0.02, 4.5, 22.4, 42.0, 40.6, 41.6, 38.0, 42.4, 38.5, 35.0, 43.4,
     46.3, 43.9, 37.1, 36.7, 35.9, 32.6, 27.9, 24.3, 20.1, 16.2, 13.2,
     8.6, 6.1, 4.2, 1.9, 0.0, -1.6, -3.5, -3.5, -5.8, -7.2, -8.6, -9.5,
     -10.9, -10.7, -12.0, -14.0, -13.6, -12.0, -13.3, -12.9, -10.6,
     -11.6, -12.2, -10.2, -7.8, -11.2, -10.4, -10.6, -9.7, -8.3, -9.3, -9.8]

cieS2 :: Spd
cieS2 = mkSpd' 300 830
   [ 0.0, 2.0, 4.0, 8.5, 7.8, 6.7, 5.3, 6.1, 3.0, 1.2, -1.1, -0.5, -0.7,
    -1.2, -2.6, -2.9, -2.8, -2.6, -2.6, -1.8, -1.5, -1.3, -1.2, -1.0,
    -0.5, -0.3, 0.0, 0.2, 0.5, 2.1, 3.2, 4.1, 4.7, 5.1, 6.7, 7.3, 8.6,
     9.8, 10.2, 8.3, 9.6, 8.5, 7.0, 7.6, 8.0, 6.7, 5.2, 7.4, 6.8, 7.0,
     6.4, 5.5, 6.1, 6.5]

--------------------------------------------------------------------------------
-- CIE XYZ SPDs
--------------------------------------------------------------------------------

cieStart :: Int
cieStart = 360

cieEnd :: Int
cieEnd = 830

cieSpd :: [Float] -> Spd
cieSpd = mkSpd' (fromIntegral cieStart) (fromIntegral cieEnd)

cieX :: Spd
cieX = cieSpd cieXValues

cieY :: Spd
cieY = cieSpd cieYValues

cieZ :: Spd
cieZ = cieSpd cieZValues

--------------------------------------------------------------------------------
-- Only boring tables below
--------------------------------------------------------------------------------

rgbToSpectrumStart :: Float
rgbToSpectrumStart = 380

rgbToSpectrumEnd :: Float
rgbToSpectrumEnd = 720

rgbFunc :: [Float] -> Spectrum
rgbFunc vs = fromSpd $ mkSpd' rgbToSpectrumStart rgbToSpectrumEnd vs

rgbIllumWhite :: Spectrum
rgbIllumWhite = rgbFunc
   [  1.1565232050369776e+00, 1.1567225000119139e+00, 1.1566203150243823e+00,
      1.1555782088080084e+00, 1.1562175509215700e+00, 1.1567674012207332e+00,
      1.1568023194808630e+00, 1.1567677445485520e+00, 1.1563563182952830e+00,
      1.1567054702510189e+00, 1.1565134139372772e+00, 1.1564336176499312e+00,
      1.1568023181530034e+00, 1.1473147688514642e+00, 1.1339317140561065e+00,
      1.1293876490671435e+00, 1.1290515328639648e+00, 1.0504864823782283e+00,
      1.0459696042230884e+00, 9.9366687168595691e-01, 9.5601669265393940e-01,
      9.2467482033511805e-01, 9.1499944702051761e-01, 8.9939467658453465e-01,
      8.9542520751331112e-01, 8.8870566693814745e-01, 8.8222843814228114e-01,
      8.7998311373826676e-01, 8.7635244612244578e-01, 8.8000368331709111e-01,
      8.8065665428441120e-01, 8.8304706460276905e-01 ]

rgbIllumCyan :: Spectrum
rgbIllumCyan = rgbFunc
   [  1.1334479663682135e+00, 1.1266762330194116e+00, 1.1346827504710164e+00,
      1.1357395805744794e+00, 1.1356371830149636e+00, 1.1361152989346193e+00,
      1.1362179057706772e+00, 1.1364819652587022e+00, 1.1355107110714324e+00,
      1.1364060941199556e+00, 1.1360363621722465e+00, 1.1360122641141395e+00,
      1.1354266882467030e+00, 1.1363099407179136e+00, 1.1355450412632506e+00,
      1.1353732327376378e+00, 1.1349496420726002e+00, 1.1111113947168556e+00,
      9.0598740429727143e-01, 6.1160780787465330e-01, 2.9539752170999634e-01,
      9.5954200671150097e-02,-1.1650792030826267e-02,-1.2144633073395025e-02,
     -1.1148167569748318e-02,-1.1997606668458151e-02,-5.0506855475394852e-03,
     -7.9982745819542154e-03,-9.4722817708236418e-03,-5.5329541006658815e-03,
     -4.5428914028274488e-03,-1.2541015360921132e-02 ]

rgbIllumMagenta :: Spectrum
rgbIllumMagenta = rgbFunc
   [  1.0371892935878366e+00, 1.0587542891035364e+00, 1.0767271213688903e+00,
      1.0762706844110288e+00, 1.0795289105258212e+00, 1.0743644742950074e+00,
      1.0727028691194342e+00, 1.0732447452056488e+00, 1.0823760816041414e+00,
      1.0840545681409282e+00, 9.5607567526306658e-01, 5.5197896855064665e-01,
      8.4191094887247575e-02, 8.7940070557041006e-05,-2.3086408335071251e-03,
     -1.1248136628651192e-03,-7.7297612754989586e-11,-2.7270769006770834e-04,
      1.4466473094035592e-02, 2.5883116027169478e-01, 5.2907999827566732e-01,
      9.0966624097105164e-01, 1.0690571327307956e+00, 1.0887326064796272e+00,
      1.0637622289511852e+00, 1.0201812918094260e+00, 1.0262196688979945e+00,
      1.0783085560613190e+00, 9.8333849623218872e-01, 1.0707246342802621e+00,
      1.0634247770423768e+00, 1.0150875475729566e+00 ]

rgbIllumYellow :: Spectrum
rgbIllumYellow = rgbFunc
   [  2.7756958965811972e-03, 3.9673820990646612e-03,-1.4606936788606750e-04,
      3.6198394557748065e-04,-2.5819258699309733e-04,-5.0133191628082274e-05,
     -2.4437242866157116e-04,-7.8061419948038946e-05, 4.9690301207540921e-02,
      4.8515973574763166e-01, 1.0295725854360589e+00, 1.0333210878457741e+00,
      1.0368102644026933e+00, 1.0364884018886333e+00, 1.0365427939411784e+00,
      1.0368595402854539e+00, 1.0365645405660555e+00, 1.0363938240707142e+00,
      1.0367205578770746e+00, 1.0365239329446050e+00, 1.0361531226427443e+00,
      1.0348785007827348e+00, 1.0042729660717318e+00, 8.4218486432354278e-01,
      7.3759394894801567e-01, 6.5853154500294642e-01, 6.0531682444066282e-01,
      5.9549794132420741e-01, 5.9419261278443136e-01, 5.6517682326634266e-01,
      5.6061186014968556e-01, 5.8228610381018719e-01 ]

rgbIllumRed :: Spectrum
rgbIllumRed = rgbFunc
   [  5.4711187157291841e-02, 5.5609066498303397e-02, 6.0755873790918236e-02,
      5.6232948615962369e-02, 4.6169940535708678e-02, 3.8012808167818095e-02,
      2.4424225756670338e-02, 3.8983580581592181e-03,-5.6082252172734437e-04,
      9.6493871255194652e-04, 3.7341198051510371e-04,-4.3367389093135200e-04,
     -9.3533962256892034e-05,-1.2354967412842033e-04,-1.4524548081687461e-04,
     -2.0047691915543731e-04,-4.9938587694693670e-04, 2.7255083540032476e-02,
      1.6067405906297061e-01, 3.5069788873150953e-01, 5.7357465538418961e-01,
      7.6392091890718949e-01, 8.9144466740381523e-01, 9.6394609909574891e-01,
      9.8879464276016282e-01, 9.9897449966227203e-01, 9.8605140403564162e-01,
      9.9532502805345202e-01, 9.7433478377305371e-01, 9.9134364616871407e-01,
      9.8866287772174755e-01, 9.9713856089735531e-01 ]

rgbIllumGreen :: Spectrum
rgbIllumGreen = rgbFunc
   [  2.5168388755514630e-02, 3.9427438169423720e-02, 6.2059571596425793e-03,
      7.1120859807429554e-03, 2.1760044649139429e-04, 7.3271839984290210e-12,
     -2.1623066217181700e-02, 1.5670209409407512e-02, 2.8019603188636222e-03,
      3.2494773799897647e-01, 1.0164917292316602e+00, 1.0329476657890369e+00,
      1.0321586962991549e+00, 1.0358667411948619e+00, 1.0151235476834941e+00,
      1.0338076690093119e+00, 1.0371372378155013e+00, 1.0361377027692558e+00,
      1.0229822432557210e+00, 9.6910327335652324e-01,-5.1785923899878572e-03,
      1.1131261971061429e-03, 6.6675503033011771e-03, 7.4024315686001957e-04,
      2.1591567633473925e-02, 5.1481620056217231e-03, 1.4561928645728216e-03,
      1.6414511045291513e-04,-6.4630764968453287e-03, 1.0250854718507939e-02,
      4.2387394733956134e-02, 2.1252716926861620e-02 ]

rgbIllumBlue :: Spectrum
rgbIllumBlue = rgbFunc
   [  1.0570490759328752e+00, 1.0538466912851301e+00, 1.0550494258140670e+00,
      1.0530407754701832e+00, 1.0579930596460185e+00, 1.0578439494812371e+00,
      1.0583132387180239e+00, 1.0579712943137616e+00, 1.0561884233578465e+00,
      1.0571399285426490e+00, 1.0425795187752152e+00, 3.2603084374056102e-01,
     -1.9255628442412243e-03,-1.2959221137046478e-03,-1.4357356276938696e-03,
     -1.2963697250337886e-03,-1.9227081162373899e-03, 1.2621152526221778e-03,
     -1.6095249003578276e-03,-1.3029983817879568e-03,-1.7666600873954916e-03,
     -1.2325281140280050e-03, 1.0316809673254932e-02, 3.1284512648354357e-02,
      8.8773879881746481e-02, 1.3873621740236541e-01, 1.5535067531939065e-01,
      1.4878477178237029e-01, 1.6624255403475907e-01, 1.6997613960634927e-01,
      1.5769743995852967e-01, 1.9069090525482305e-01 ]

rgbReflWhite :: Spectrum
rgbReflWhite = rgbFunc
   [  1.0618958571272863e+00, 1.0615019980348779e+00, 1.0614335379927147e+00,
      1.0622711654692485e+00, 1.0622036218416742e+00, 1.0625059965187085e+00,
      1.0623938486985884e+00, 1.0624706448043137e+00, 1.0625048144827762e+00,
      1.0624366131308856e+00, 1.0620694238892607e+00, 1.0613167586932164e+00,
      1.0610334029377020e+00, 1.0613868564828413e+00, 1.0614215366116762e+00,
      1.0620336151299086e+00, 1.0625497454805051e+00, 1.0624317487992085e+00,
      1.0625249140554480e+00, 1.0624277664486914e+00, 1.0624749854090769e+00,
      1.0625538581025402e+00, 1.0625326910104864e+00, 1.0623922312225325e+00,
      1.0623650980354129e+00, 1.0625256476715284e+00, 1.0612277619533155e+00,
      1.0594262608698046e+00, 1.0599810758292072e+00, 1.0602547314449409e+00,
      1.0601263046243634e+00, 1.0606565756823634e+00 ]

rgbReflCyan :: Spectrum
rgbReflCyan = rgbFunc
   [  1.0414628021426751e+00, 1.0328661533771188e+00, 1.0126146228964314e+00,
      1.0350460524836209e+00, 1.0078661447098567e+00, 1.0422280385081280e+00,
      1.0442596738499825e+00, 1.0535238290294409e+00, 1.0180776226938120e+00,
      1.0442729908727713e+00, 1.0529362541920750e+00, 1.0537034271160244e+00,
      1.0533901869215969e+00, 1.0537782700979574e+00, 1.0527093770467102e+00,
      1.0530449040446797e+00, 1.0550554640191208e+00, 1.0553673610724821e+00,
      1.0454306634683976e+00, 6.2348950639230805e-01, 1.8038071613188977e-01,
     -7.6303759201984539e-03,-1.5217847035781367e-04,-7.5102257347258311e-03,
     -2.1708639328491472e-03, 6.5919466602369636e-04, 1.2278815318539780e-02,
     -4.4669775637208031e-03, 1.7119799082865147e-02, 4.9211089759759801e-03,
      5.8762925143334985e-03, 2.5259399415550079e-02 ]

rgbReflMagenta :: Spectrum
rgbReflMagenta = rgbFunc
   [  9.9422138151236850e-01, 9.8986937122975682e-01, 9.8293658286116958e-01,
      9.9627868399859310e-01, 1.0198955019000133e+00, 1.0166395501210359e+00,
      1.0220913178757398e+00, 9.9651666040682441e-01, 1.0097766178917882e+00,
      1.0215422470827016e+00, 6.4031953387790963e-01, 2.5012379477078184e-03,
      6.5339939555769944e-03, 2.8334080462675826e-03,-5.1209675389074505e-11,
     -9.0592291646646381e-03, 3.3936718323331200e-03,-3.0638741121828406e-03,
      2.2203936168286292e-01, 6.3141140024811970e-01, 9.7480985576500956e-01,
      9.7209562333590571e-01, 1.0173770302868150e+00, 9.9875194322734129e-01,
      9.4701725739602238e-01, 8.5258623154354796e-01, 9.4897798581660842e-01,
      9.4751876096521492e-01, 9.9598944191059791e-01, 8.6301351503809076e-01,
      8.9150987853523145e-01, 8.4866492652845082e-01 ]

rgbReflYellow :: Spectrum
rgbReflYellow = rgbFunc
   [  5.5740622924920873e-03,-4.7982831631446787e-03,
     -5.2536564298613798e-03,-6.4571480044499710e-03,
     -5.9693514658007013e-03,-2.1836716037686721e-03,
      1.6781120601055327e-02, 9.6096355429062641e-02,
      2.1217357081986446e-01, 3.6169133290685068e-01,
      5.3961011543232529e-01, 7.4408810492171507e-01,
      9.2209571148394054e-01, 1.0460304298411225e+00,
      1.0513824989063714e+00, 1.0511991822135085e+00,
      1.0510530911991052e+00, 1.0517397230360510e+00,
      1.0516043086790485e+00, 1.0511944032061460e+00,
      1.0511590325868068e+00, 1.0516612465483031e+00,
      1.0514038526836869e+00, 1.0515941029228475e+00,
      1.0511460436960840e+00, 1.0515123758830476e+00,
      1.0508871369510702e+00, 1.0508923708102380e+00,
      1.0477492815668303e+00, 1.0493272144017338e+00,
      1.0435963333422726e+00, 1.0392280772051465e+00 ]

rgbReflRed :: Spectrum
rgbReflRed = rgbFunc
   [  1.6575604867086180e-01, 1.1846442802747797e-01,
      1.2408293329637447e-01, 1.1371272058349924e-01,
      7.8992434518899132e-02, 3.2205603593106549e-02,
     -1.0798365407877875e-02, 1.8051975516730392e-02,
      5.3407196598730527e-03, 1.3654918729501336e-02,
     -5.9564213545642841e-03, -1.8444365067353252e-03,
     -1.0571884361529504e-02, -2.9375521078000011e-03,
     -1.0790476271835936e-02, -8.0224306697503633e-03,
     -2.2669167702495940e-03, 7.0200240494706634e-03,
     -8.1528469000299308e-03, 6.0772866969252792e-01,
      9.8831560865432400e-01, 9.9391691044078823e-01,
      1.0039338994753197e+00, 9.9234499861167125e-01,
      9.9926530858855522e-01, 1.0084621557617270e+00,
      9.8358296827441216e-01, 1.0085023660099048e+00,
      9.7451138326568698e-01, 9.8543269570059944e-01,
      9.3495763980962043e-01, 9.8713907792319400e-01 ]

rgbReflGreen :: Spectrum
rgbReflGreen = rgbFunc
   [  2.6494153587602255e-03, -5.0175013429732242e-03,
     -1.2547236272489583e-02, -9.4554964308388671e-03,
     -1.2526086181600525e-02, -7.9170697760437767e-03,
     -7.9955735204175690e-03, -9.3559433444469070e-03,
      6.5468611982999303e-02, 3.9572875517634137e-01,
      7.5244022299886659e-01, 9.6376478690218559e-01,
      9.9854433855162328e-01, 9.9992977025287921e-01,
      9.9939086751140449e-01, 9.9994372267071396e-01,
      9.9939121813418674e-01, 9.9911237310424483e-01,
      9.6019584878271580e-01, 6.3186279338432438e-01,
      2.5797401028763473e-01, 9.4014888527335638e-03,
     -3.0798345608649747e-03, -4.5230367033685034e-03,
     -6.8933410388274038e-03, -9.0352195539015398e-03,
     -8.5913667165340209e-03, -8.3690869120289398e-03,
     -7.8685832338754313e-03, -8.3657578711085132e-06,
      5.4301225442817177e-03, -2.7745589759259194e-03 ]

rgbReflBlue :: Spectrum
rgbReflBlue = rgbFunc
   [  9.9209771469720676e-01, 9.8876426059369127e-01,
      9.9539040744505636e-01, 9.9529317353008218e-01,
      9.9181447411633950e-01, 1.0002584039673432e+00,
      9.9968478437342512e-01, 9.9988120766657174e-01,
      9.8504012146370434e-01, 7.9029849053031276e-01,
      5.6082198617463974e-01, 3.3133458513996528e-01,
      1.3692410840839175e-01, 1.8914906559664151e-02,
     -5.1129770932550889e-06, -4.2395493167891873e-04,
     -4.1934593101534273e-04, 1.7473028136486615e-03,
      3.7999160177631316e-03, -5.5101474906588642e-04,
     -4.3716662898480967e-05, 7.5874501748732798e-03,
      2.5795650780554021e-02, 3.8168376532500548e-02,
      4.9489586408030833e-02, 4.9595992290102905e-02,
      4.9814819505812249e-02, 3.9840911064978023e-02,
      3.0501024937233868e-02, 2.1243054765241080e-02,
      6.9596532104356399e-03, 4.1733649330980525e-03 ]

cieXValues :: [Float]
{-# NOINLINE cieXValues #-}
cieXValues = [
   0.0001299000, 0.0001458470, 0.0001638021, 0.0001840037,
   0.0002066902,  0.0002321000,  0.0002607280,  0.0002930750,
   0.0003293880,  0.0003699140,  0.0004149000,  0.0004641587,
   0.0005189860,  0.0005818540,  0.0006552347,  0.0007416000,
   0.0008450296,  0.0009645268,  0.001094949,  0.001231154,
   0.001368000,  0.001502050,  0.001642328,  0.001802382,
   0.001995757,  0.002236000,  0.002535385,  0.002892603,
   0.003300829,  0.003753236,  0.004243000,  0.004762389,
   0.005330048,  0.005978712,  0.006741117,  0.007650000,
   0.008751373,  0.01002888,  0.01142170,  0.01286901,
   0.01431000,  0.01570443,  0.01714744,  0.01878122,
   0.02074801,  0.02319000,  0.02620736,  0.02978248,
   0.03388092,  0.03846824,  0.04351000,  0.04899560,
   0.05502260,  0.06171880,  0.06921200,  0.07763000,
   0.08695811,  0.09717672,  0.1084063,  0.1207672,
   0.1343800,  0.1493582,  0.1653957,  0.1819831,
   0.1986110,  0.2147700,  0.2301868,  0.2448797,
   0.2587773,  0.2718079,  0.2839000,  0.2949438,
   0.3048965,  0.3137873,  0.3216454,  0.3285000,
   0.3343513,  0.3392101,  0.3431213,  0.3461296,
   0.3482800,  0.3495999,  0.3501474,  0.3500130,
   0.3492870,  0.3480600,  0.3463733,  0.3442624,
   0.3418088,  0.3390941,  0.3362000,  0.3331977,
   0.3300411,  0.3266357,  0.3228868,  0.3187000,
   0.3140251,  0.3088840,  0.3032904,  0.2972579,
   0.2908000,  0.2839701,  0.2767214,  0.2689178,
   0.2604227,  0.2511000,  0.2408475,  0.2298512,
   0.2184072,  0.2068115,  0.1953600,  0.1842136,
   0.1733273,  0.1626881,  0.1522833,  0.1421000,
   0.1321786,  0.1225696,  0.1132752,  0.1042979,
   0.09564000,  0.08729955,  0.07930804,  0.07171776,
   0.06458099,  0.05795001,  0.05186211,  0.04628152,
   0.04115088,  0.03641283,  0.03201000,  0.02791720,
   0.02414440,  0.02068700,  0.01754040,  0.01470000,
   0.01216179,  0.009919960,  0.007967240,  0.006296346,
   0.004900000,  0.003777173,  0.002945320,  0.002424880,
   0.002236293,  0.002400000,  0.002925520,  0.003836560,
   0.005174840,  0.006982080,  0.009300000,  0.01214949,
   0.01553588,  0.01947752,  0.02399277,  0.02910000,
   0.03481485,  0.04112016,  0.04798504,  0.05537861,
   0.06327000,  0.07163501,  0.08046224,  0.08973996,
   0.09945645,  0.1096000,  0.1201674,  0.1311145,
   0.1423679,  0.1538542,  0.1655000,  0.1772571,
   0.1891400,  0.2011694,  0.2133658,  0.2257499,
   0.2383209,  0.2510668,  0.2639922,  0.2771017,
   0.2904000,  0.3038912,  0.3175726,  0.3314384,
   0.3454828,  0.3597000,  0.3740839,  0.3886396,
   0.4033784,  0.4183115,  0.4334499,  0.4487953,
   0.4643360,  0.4800640,  0.4959713,  0.5120501,
   0.5282959,  0.5446916,  0.5612094,  0.5778215,
   0.5945000,  0.6112209,  0.6279758,  0.6447602,
   0.6615697,  0.6784000,  0.6952392,  0.7120586,
   0.7288284,  0.7455188,  0.7621000,  0.7785432,
   0.7948256,  0.8109264,  0.8268248,  0.8425000,
   0.8579325,  0.8730816,  0.8878944,  0.9023181,
   0.9163000,  0.9297995,  0.9427984,  0.9552776,
   0.9672179,  0.9786000,  0.9893856,  0.9995488,
   1.0090892,  1.0180064,  1.0263000,  1.0339827,
   1.0409860,  1.0471880,  1.0524667,  1.0567000,
   1.0597944,  1.0617992,  1.0628068,  1.0629096,
   1.0622000,  1.0607352,  1.0584436,  1.0552244,
   1.0509768,  1.0456000,  1.0390369,  1.0313608,
   1.0226662,  1.0130477,  1.0026000,  0.9913675,
   0.9793314,  0.9664916,  0.9528479,  0.9384000,
   0.9231940,  0.9072440,  0.8905020,  0.8729200,
   0.8544499,  0.8350840,  0.8149460,  0.7941860,
   0.7729540,  0.7514000,  0.7295836,  0.7075888,
   0.6856022,  0.6638104,  0.6424000,  0.6215149,
   0.6011138,  0.5811052,  0.5613977,  0.5419000,
   0.5225995,  0.5035464,  0.4847436,  0.4661939,
   0.4479000,  0.4298613,  0.4120980,  0.3946440,
   0.3775333,  0.3608000,  0.3444563,  0.3285168,
   0.3130192,  0.2980011,  0.2835000,  0.2695448,
   0.2561184,  0.2431896,  0.2307272,  0.2187000,
   0.2070971,  0.1959232,  0.1851708,  0.1748323,
   0.1649000,  0.1553667,  0.1462300,  0.1374900,
   0.1291467,  0.1212000,  0.1136397,  0.1064650,
   0.09969044,  0.09333061,  0.08740000,  0.08190096,
   0.07680428,  0.07207712,  0.06768664,  0.06360000,
   0.05980685,  0.05628216,  0.05297104,  0.04981861,
   0.04677000,  0.04378405,  0.04087536,  0.03807264,
   0.03540461,  0.03290000,  0.03056419,  0.02838056,
   0.02634484,  0.02445275,  0.02270000,  0.02108429,
   0.01959988,  0.01823732,  0.01698717,  0.01584000,
   0.01479064,  0.01383132,  0.01294868,  0.01212920,
   0.01135916,  0.01062935,  0.009938846,  0.009288422,
   0.008678854,  0.008110916,  0.007582388,  0.007088746,
   0.006627313,  0.006195408,  0.005790346,  0.005409826,
   0.005052583,  0.004717512,  0.004403507,  0.004109457,
   0.003833913,  0.003575748,  0.003334342,  0.003109075,
   0.002899327,  0.002704348,  0.002523020,  0.002354168,
   0.002196616,  0.002049190,  0.001910960,  0.001781438,
   0.001660110,  0.001546459,  0.001439971,  0.001340042,
   0.001246275,  0.001158471,  0.001076430,  0.0009999493,
   0.0009287358,  0.0008624332,  0.0008007503,  0.0007433960,
   0.0006900786,  0.0006405156,  0.0005945021,  0.0005518646,
   0.0005124290,  0.0004760213,  0.0004424536,  0.0004115117,
   0.0003829814,  0.0003566491,  0.0003323011,  0.0003097586,
   0.0002888871,  0.0002695394,  0.0002515682,  0.0002348261,
   0.0002191710,  0.0002045258,  0.0001908405,  0.0001780654,
   0.0001661505,  0.0001550236,  0.0001446219,  0.0001349098,
   0.0001258520,  0.0001174130,  0.0001095515,  0.0001022245,
   0.00009539445,  0.00008902390,  0.00008307527,  0.00007751269,
   0.00007231304,  0.00006745778,  0.00006292844,  0.00005870652,
   0.00005477028,  0.00005109918,  0.00004767654,  0.00004448567,
   0.00004150994,  0.00003873324,  0.00003614203,  0.00003372352,
   0.00003146487,  0.00002935326,  0.00002737573,  0.00002552433,
   0.00002379376,  0.00002217870,  0.00002067383,  0.00001927226,
   0.00001796640,  0.00001674991,  0.00001561648,  0.00001455977,
   0.00001357387,  0.00001265436,  0.00001179723,  0.00001099844,
   0.00001025398,  0.000009559646,  0.000008912044,  0.000008308358,
   0.000007745769,  0.000007221456,  0.000006732475,  0.000006276423,
   0.000005851304,  0.000005455118,  0.000005085868,  0.000004741466,
   0.000004420236,  0.000004120783,  0.000003841716,  0.000003581652,
   0.000003339127,  0.000003112949,  0.000002902121,  0.000002705645,
   0.000002522525,  0.000002351726,  0.000002192415,  0.000002043902,
   0.000001905497,  0.000001776509,  0.000001656215,  0.000001544022,
   0.000001439440, 0.000001341977, 0.000001251141]

cieYValues :: [Float]
{-# NOINLINE cieYValues #-}
cieYValues = [
   0.000003917000,  0.000004393581,  0.000004929604,  0.000005532136,
   0.000006208245,  0.000006965000,  0.000007813219,  0.000008767336,
   0.000009839844,  0.00001104323,  0.00001239000,  0.00001388641,
   0.00001555728,  0.00001744296,  0.00001958375,  0.00002202000,
   0.00002483965,  0.00002804126,  0.00003153104,  0.00003521521,
   0.00003900000,  0.00004282640,  0.00004691460,  0.00005158960,
   0.00005717640,  0.00006400000,  0.00007234421,  0.00008221224,
   0.00009350816,  0.0001061361,  0.0001200000,  0.0001349840,
   0.0001514920,  0.0001702080,  0.0001918160,  0.0002170000,
   0.0002469067,  0.0002812400,  0.0003185200,  0.0003572667,
   0.0003960000,  0.0004337147,  0.0004730240,  0.0005178760,
   0.0005722187,  0.0006400000,  0.0007245600,  0.0008255000,
   0.0009411600,  0.001069880,  0.001210000,  0.001362091,
   0.001530752,  0.001720368,  0.001935323,  0.002180000,
   0.002454800,  0.002764000,  0.003117800,  0.003526400,
   0.004000000,  0.004546240,  0.005159320,  0.005829280,
   0.006546160,  0.007300000,  0.008086507,  0.008908720,
   0.009767680,  0.01066443,  0.01160000,  0.01257317,
   0.01358272,  0.01462968,  0.01571509,  0.01684000,
   0.01800736,  0.01921448,  0.02045392,  0.02171824,
   0.02300000,  0.02429461,  0.02561024,  0.02695857,
   0.02835125,  0.02980000,  0.03131083,  0.03288368,
   0.03452112,  0.03622571,  0.03800000,  0.03984667,
   0.04176800,  0.04376600,  0.04584267,  0.04800000,
   0.05024368,  0.05257304,  0.05498056,  0.05745872,
   0.06000000,  0.06260197,  0.06527752,  0.06804208,
   0.07091109,  0.07390000,  0.07701600,  0.08026640,
   0.08366680,  0.08723280,  0.09098000,  0.09491755,
   0.09904584,  0.1033674,  0.1078846,  0.1126000,
   0.1175320,  0.1226744,  0.1279928,  0.1334528,
   0.1390200,  0.1446764,  0.1504693,  0.1564619,
   0.1627177,  0.1693000,  0.1762431,  0.1835581,
   0.1912735,  0.1994180,  0.2080200,  0.2171199,
   0.2267345,  0.2368571,  0.2474812,  0.2586000,
   0.2701849,  0.2822939,  0.2950505,  0.3085780,
   0.3230000,  0.3384021,  0.3546858,  0.3716986,
   0.3892875,  0.4073000,  0.4256299,  0.4443096,
   0.4633944,  0.4829395,  0.5030000,  0.5235693,
   0.5445120,  0.5656900,  0.5869653,  0.6082000,
   0.6293456,  0.6503068,  0.6708752,  0.6908424,
   0.7100000,  0.7281852,  0.7454636,  0.7619694,
   0.7778368,  0.7932000,  0.8081104,  0.8224962,
   0.8363068,  0.8494916,  0.8620000,  0.8738108,
   0.8849624,  0.8954936,  0.9054432,  0.9148501,
   0.9237348,  0.9320924,  0.9399226,  0.9472252,
   0.9540000,  0.9602561,  0.9660074,  0.9712606,
   0.9760225,  0.9803000,  0.9840924,  0.9874812,
   0.9903128,  0.9928116,  0.9949501,  0.9967108,
   0.9980983,  0.9991120,  0.9997482,  1.0000000,
   0.9998567,  0.9993046,  0.9983255,  0.9968987,
   0.9950000,  0.9926005,  0.9897426,  0.9864444,
   0.9827241,  0.9786000,  0.9740837,  0.9691712,
   0.9638568,  0.9581349,  0.9520000,  0.9454504,
   0.9384992,  0.9311628,  0.9234576,  0.9154000,
   0.9070064,  0.8982772,  0.8892048,  0.8797816,
   0.8700000,  0.8598613,  0.8493920,  0.8386220,
   0.8275813,  0.8163000,  0.8047947,  0.7930820,
   0.7811920,  0.7691547,  0.7570000,  0.7447541,
   0.7324224,  0.7200036,  0.7074965,  0.6949000,
   0.6822192,  0.6694716,  0.6566744,  0.6438448,
   0.6310000,  0.6181555,  0.6053144,  0.5924756,
   0.5796379,  0.5668000,  0.5539611,  0.5411372,
   0.5283528,  0.5156323,  0.5030000,  0.4904688,
   0.4780304,  0.4656776,  0.4534032,  0.4412000,
   0.4290800,  0.4170360,  0.4050320,  0.3930320,
   0.3810000,  0.3689184,  0.3568272,  0.3447768,
   0.3328176,  0.3210000,  0.3093381,  0.2978504,
   0.2865936,  0.2756245,  0.2650000,  0.2547632,
   0.2448896,  0.2353344,  0.2260528,  0.2170000,
   0.2081616,  0.1995488,  0.1911552,  0.1829744,
   0.1750000,  0.1672235,  0.1596464,  0.1522776,
   0.1451259,  0.1382000,  0.1315003,  0.1250248,
   0.1187792,  0.1127691,  0.1070000,  0.1014762,
   0.09618864,  0.09112296,  0.08626485,  0.08160000,
   0.07712064,  0.07282552,  0.06871008,  0.06476976,
   0.06100000,  0.05739621,  0.05395504,  0.05067376,
   0.04754965,  0.04458000,  0.04175872,  0.03908496,
   0.03656384,  0.03420048,  0.03200000,  0.02996261,
   0.02807664,  0.02632936,  0.02470805,  0.02320000,
   0.02180077,  0.02050112,  0.01928108,  0.01812069,
   0.01700000,  0.01590379,  0.01483718,  0.01381068,
   0.01283478,  0.01192000,  0.01106831,  0.01027339,
   0.009533311,  0.008846157,  0.008210000,  0.007623781,
   0.007085424,  0.006591476,  0.006138485,  0.005723000,
   0.005343059,  0.004995796,  0.004676404,  0.004380075,
   0.004102000,  0.003838453,  0.003589099,  0.003354219,
   0.003134093,  0.002929000,  0.002738139,  0.002559876,
   0.002393244,  0.002237275,  0.002091000,  0.001953587,
   0.001824580,  0.001703580,  0.001590187,  0.001484000,
   0.001384496,  0.001291268,  0.001204092,  0.001122744,
   0.001047000,  0.0009765896,  0.0009111088,  0.0008501332,
   0.0007932384,  0.0007400000,  0.0006900827,  0.0006433100,
   0.0005994960,  0.0005584547,  0.0005200000,  0.0004839136,
   0.0004500528,  0.0004183452,  0.0003887184,  0.0003611000,
   0.0003353835,  0.0003114404,  0.0002891656,  0.0002684539,
   0.0002492000,  0.0002313019,  0.0002146856,  0.0001992884,
   0.0001850475,  0.0001719000,  0.0001597781,  0.0001486044,
   0.0001383016,  0.0001287925,  0.0001200000,  0.0001118595,
   0.0001043224,  0.00009733560,  0.00009084587,  0.00008480000,
   0.00007914667,  0.00007385800,  0.00006891600,  0.00006430267,
   0.00006000000,  0.00005598187,  0.00005222560,  0.00004871840,
   0.00004544747,  0.00004240000,  0.00003956104,  0.00003691512,
   0.00003444868,  0.00003214816,  0.00003000000,  0.00002799125,
   0.00002611356,  0.00002436024,  0.00002272461,  0.00002120000,
   0.00001977855,  0.00001845285,  0.00001721687,  0.00001606459,
   0.00001499000,  0.00001398728,  0.00001305155,  0.00001217818,
   0.00001136254,  0.00001060000,  0.000009885877,  0.000009217304,
   0.000008592362,  0.000008009133,  0.000007465700,  0.000006959567,
   0.000006487995,  0.000006048699,  0.000005639396,  0.000005257800,
   0.000004901771,  0.000004569720,  0.000004260194,  0.000003971739,
   0.000003702900,  0.000003452163,  0.000003218302,  0.000003000300,
   0.000002797139,  0.000002607800,  0.000002431220,  0.000002266531,
   0.000002113013,  0.000001969943,  0.000001836600,  0.000001712230,
   0.000001596228,  0.000001488090,  0.000001387314,  0.000001293400,
   0.000001205820,  0.000001124143,  0.000001048009,  0.0000009770578,
   0.0000009109300,  0.0000008492513,  0.0000007917212,  0.0000007380904,
   0.0000006881098,  0.0000006415300,  0.0000005980895,  0.0000005575746,
   0.0000005198080, 0.0000004846123, 0.0000004518100 ]

cieZValues :: [Float]
{-# NOINLINE cieZValues #-}
cieZValues = [
   0.0006061000,  0.0006808792,  0.0007651456,  0.0008600124,
   0.0009665928,  0.001086000,  0.001220586,  0.001372729,
   0.001543579,  0.001734286,  0.001946000,  0.002177777,
   0.002435809,  0.002731953,  0.003078064,  0.003486000,
   0.003975227,  0.004540880,  0.005158320,  0.005802907,
   0.006450001,  0.007083216,  0.007745488,  0.008501152,
   0.009414544,  0.01054999,  0.01196580,  0.01365587,
   0.01558805,  0.01773015,  0.02005001,  0.02251136,
   0.02520288,  0.02827972,  0.03189704,  0.03621000,
   0.04143771,  0.04750372,  0.05411988,  0.06099803,
   0.06785001,  0.07448632,  0.08136156,  0.08915364,
   0.09854048,  0.1102000,  0.1246133,  0.1417017,
   0.1613035,  0.1832568,  0.2074000,  0.2336921,
   0.2626114,  0.2947746,  0.3307985,  0.3713000,
   0.4162091,  0.4654642,  0.5196948,  0.5795303,
   0.6456000,  0.7184838,  0.7967133,  0.8778459,
   0.9594390,  1.0390501,  1.1153673,  1.1884971,
   1.2581233,  1.3239296,  1.3856000,  1.4426352,
   1.4948035,  1.5421903,  1.5848807,  1.6229600,
   1.6564048,  1.6852959,  1.7098745,  1.7303821,
   1.7470600,  1.7600446,  1.7696233,  1.7762637,
   1.7804334,  1.7826000,  1.7829682,  1.7816998,
   1.7791982,  1.7758671,  1.7721100,  1.7682589,
   1.7640390,  1.7589438,  1.7524663,  1.7441000,
   1.7335595,  1.7208581,  1.7059369,  1.6887372,
   1.6692000,  1.6475287,  1.6234127,  1.5960223,
   1.5645280,  1.5281000,  1.4861114,  1.4395215,
   1.3898799,  1.3387362,  1.2876400,  1.2374223,
   1.1878243,  1.1387611,  1.0901480,  1.0419000,
   0.9941976,  0.9473473,  0.9014531,  0.8566193,
   0.8129501,  0.7705173,  0.7294448,  0.6899136,
   0.6521049,  0.6162000,  0.5823286,  0.5504162,
   0.5203376,  0.4919673,  0.4651800,  0.4399246,
   0.4161836,  0.3938822,  0.3729459,  0.3533000,
   0.3348578,  0.3175521,  0.3013375,  0.2861686,
   0.2720000,  0.2588171,  0.2464838,  0.2347718,
   0.2234533,  0.2123000,  0.2011692,  0.1901196,
   0.1792254,  0.1685608,  0.1582000,  0.1481383,
   0.1383758,  0.1289942,  0.1200751,  0.1117000,
   0.1039048,  0.09666748,  0.08998272,  0.08384531,
   0.07824999,  0.07320899,  0.06867816,  0.06456784,
   0.06078835,  0.05725001,  0.05390435,  0.05074664,
   0.04775276,  0.04489859,  0.04216000,  0.03950728,
   0.03693564,  0.03445836,  0.03208872,  0.02984000,
   0.02771181,  0.02569444,  0.02378716,  0.02198925,
   0.02030000,  0.01871805,  0.01724036,  0.01586364,
   0.01458461,  0.01340000,  0.01230723,  0.01130188,
   0.01037792,  0.009529306,  0.008749999,  0.008035200,
   0.007381600,  0.006785400,  0.006242800,  0.005749999,
   0.005303600,  0.004899800,  0.004534200,  0.004202400,
   0.003900000,  0.003623200,  0.003370600,  0.003141400,
   0.002934800,  0.002749999,  0.002585200,  0.002438600,
   0.002309400,  0.002196800,  0.002100000,  0.002017733,
   0.001948200,  0.001889800,  0.001840933,  0.001800000,
   0.001766267,  0.001737800,  0.001711200,  0.001683067,
   0.001650001,  0.001610133,  0.001564400,  0.001513600,
   0.001458533,  0.001400000,  0.001336667,  0.001270000,
   0.001205000,  0.001146667,  0.001100000,  0.001068800,
   0.001049400,  0.001035600,  0.001021200,  0.001000000,
   0.0009686400,  0.0009299200,  0.0008868800,  0.0008425600,
   0.0008000000,  0.0007609600,  0.0007236800,  0.0006859200,
   0.0006454400,  0.0006000000,  0.0005478667,  0.0004916000,
   0.0004354000,  0.0003834667,  0.0003400000,  0.0003072533,
   0.0002831600,  0.0002654400,  0.0002518133,  0.0002400000,
   0.0002295467,  0.0002206400,  0.0002119600,  0.0002021867,
   0.0001900000,  0.0001742133,  0.0001556400,  0.0001359600,
   0.0001168533,  0.0001000000,  0.00008613333,  0.00007460000,
   0.00006500000,  0.00005693333,  0.00004999999,  0.00004416000,
   0.00003948000,  0.00003572000,  0.00003264000,  0.00003000000,
   0.00002765333,  0.00002556000,  0.00002364000,  0.00002181333,
   0.00002000000,  0.00001813333,  0.00001620000,  0.00001420000,
   0.00001213333,  0.00001000000,  0.000007733333,  0.000005400000,
   0.000003200000,  0.000001333333,  0.000000000000,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0,  0.0,
   0.0,  0.0,  0.0 ]
