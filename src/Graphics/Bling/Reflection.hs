{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Bling.Reflection (
   module Graphics.Bling.DifferentialGeometry,
   
   -- * Materials and Bump Mapping

   Material, blackBodyMaterial, bumpMapped,
   
   -- * Fresnel incidence effects
   
   Fresnel, frDielectric, frConductor, frNoOp, FresnelBlend, mkFresnelBlend,
   
   -- * Microfacet Distribution based BRDF
   
   Microfacet(..), Distribution, mkBlinn, mkAnisotropic,

   -- * BxDF Functions

   AnyBxdf(..), Bxdf(..),  Lambertian(..), mkOrenNayar, OrenNayar,
   
   -- ** BxDF Types
   BxdfProp(..), BxdfType, mkBxdfType, bxdfIs,
   
   -- * BSDF
   
   Bsdf, BsdfSample(..), mkBsdf, mkBsdf', evalBsdf, sampleBsdf, sampleBsdf',
   bsdfPdf, sampleAdjBsdf, sampleAdjBsdf',

   -- ** Querying BSDF properties
   bsdfHasNonSpecular, bsdfShadingNormal, bsdfShadingPoint, bsdfSpecCompCount,
   
   -- * Working with Vectors in shading coordinate system

   cosTheta, absCosTheta, sinTheta2, sinTheta, cosPhi, sinPhi,
   sameHemisphere
   
   ) where

import Data.Bits
import Data.List (foldl')
import qualified Data.Vector as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Montecarlo
import Graphics.Bling.Random
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture

-- | a material can turn a geometric DG and a shading DG into a BSDF
type Material = DifferentialGeometry -> DifferentialGeometry -> Bsdf

--------------------------------------------------------------------------------
-- Fresnel incidence effects
--------------------------------------------------------------------------------

type Fresnel = Flt -> Spectrum

-- | a no-op Fresnel implementation, which always returns white
--   @Spectrum@
frNoOp :: Fresnel
frNoOp = const white

-- | Fresnel incidence effects for dielectrics
frDielectric :: Flt -> Flt -> Fresnel
frDielectric etai etat cosi
   | sint >= 1 = white -- total internal reflection
   | otherwise = frDiel' (abs cosi') cost (sConst ei) (sConst et)
   where
      cosi' = clamp cosi (-1) 1
      (ei, et) = if cosi' > 0 then (etai, etat) else (etat, etai)
      -- find sint using Snell's law
      sint = (ei / et) * sqrt (max 0 (1 - cosi' * cosi'))
      cost = sqrt $ max 0 (1 - sint * sint)
      
frDiel' :: Flt -> Flt -> Spectrum -> Spectrum -> Spectrum
frDiel' cosi cost etai etat = sScale (rPar * rPar + rPer * rPer) 0.5 where
   rPar = (sScale etat cosi - sScale etai cost) /
          (sScale etat cosi + sScale etai cost)
   rPer = (sScale etai cosi - sScale etat cost) /
          (sScale etai cosi + sScale etat cost)

-- | Fresnel incidence effects for conductors
frConductor :: Spectrum -> Spectrum -> Fresnel
frConductor eta k cosi = (rPer2 + rPar2) / 2 where
   rPer2 = (tmpF - ec2 + sConst (acosi * acosi)) /
           (tmpF + ec2 + sConst (acosi * acosi))
   rPar2 = (tmp - ec2 + white) /
           (tmp + ec2 + white)
   ec2 = sScale eta (2 * acosi)
   tmp = sScale (eta * eta + k * k) (acosi * acosi)
   tmpF = eta * eta + k * k
   acosi = abs cosi
   
data FresnelBlend = FB !Spectrum !Spectrum !Distribution

mkFresnelBlend :: Spectrum -> Spectrum -> Distribution -> FresnelBlend
mkFresnelBlend = FB

instance Bxdf FresnelBlend where
   bxdfType _ = mkBxdfType [Reflection, Glossy]
   
   bxdfEval (FB rd rs d) wo wi
      | vx wh' == 0 && vy wh' == 0 && vz wh' == 0 = black
      | otherwise = diff + spec
      where
         costi = absCosTheta wi
         costo = absCosTheta wo
         wh' = wi + wo
         wh = normalize $ wh'
         diff = sScale (rd * (white - rs)) $ (28 / 23 * pi) *
                  (1 - ((1 - 0.5 * costi) ** 5)) *
                  (1 - ((1 - 0.5 * costo) ** 5))

         spec = sScale schlick $ mfDistD d wh / (4 * wi `absDot` wh) * (max costi costo)
         cost = wi `dot` wh
         schlick = rs + sScale (white - rs) ((1 - cost) ** 5)
         
   bxdfSample fb@(FB _ _ d) wo (u1, u2) = (f, wi, pdf) where
      pdf = bxdfPdf fb wo wi
      f = bxdfEval fb wo wi
      wi = if u1 < 0.5
              then toSameHemisphere wo $ cosineSampleHemisphere (u1 * 2, u2)
              else snd $ mfDistSample d (2 * (u1 - 0.5), u2) wo
              
   bxdfPdf (FB _ _ d) wo wi
      | sameHemisphere wo wi = 0.5 * (absCosTheta wi * invPi + mfDistPdf d wo wi)
      | otherwise = 0

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

cosTheta :: Vector -> Flt
{-# INLINE cosTheta #-}
cosTheta = vz

absCosTheta :: Vector -> Flt
{-# INLINE absCosTheta #-}
absCosTheta = abs . cosTheta

sinTheta2 :: Vector -> Flt
{-# INLINE sinTheta2 #-}
sinTheta2 v = max 0 (1 - cosTheta v * cosTheta v)

sinTheta :: Vector -> Flt
{-# INLINE sinTheta #-}
sinTheta = sqrt . sinTheta2

cosPhi :: Vector -> Flt
{-# INLINE cosPhi #-}
cosPhi v
   | sint == 0 = 1
   | otherwise = clamp (vx v / sint) (-1) 1
   where
         sint = sinTheta v

sinPhi :: Vector -> Flt
{-# INLINE sinPhi #-}
sinPhi v
   | sint == 0 = 0
   | otherwise = clamp (vy v / sint) (-1) 1
   where
         sint = sinTheta v

-- | decides if two vectors in shading coordinate system lie within the
--   same hemisphere
sameHemisphere :: Vector -> Vector -> Bool
{-# INLINE sameHemisphere #-}
sameHemisphere (Vector _ _ z1) (Vector _ _ z2) = z1 * z2 > 0

toSameHemisphere :: Vector -> Vector -> Vector
{-# INLINE toSameHemisphere #-}
toSameHemisphere wo wi = if vz wo < 0 then wi {vz = -(vz wi)} else wi

--------------------------------------------------------------------------------
-- BxDFs
--------------------------------------------------------------------------------

data BxdfProp
   = Reflection
   | Transmission
   | Diffuse
   | Glossy
   | Specular
   deriving (Eq, Show)

fromBxdfProp :: BxdfProp -> Int
{-# INLINE fromBxdfProp #-}
fromBxdfProp Reflection    = 1
fromBxdfProp Transmission  = 2
fromBxdfProp Diffuse       = 4
fromBxdfProp Glossy        = 8
fromBxdfProp Specular      = 16

newtype BxdfType = BxdfType { unBxdfType :: Int } deriving (Show)

bxdfIs :: BxdfType -> BxdfProp -> Bool
{-# INLINE bxdfIs #-}
bxdfIs b p = ((fromBxdfProp p) .&. (unBxdfType b)) /= 0

bxdfIs' :: BxdfType -> BxdfType -> Bool
{-# INLINE bxdfIs' #-}
bxdfIs' t1 t2 = ((unBxdfType t1) .&. (unBxdfType t2)) == (unBxdfType t1)

isReflection :: (Bxdf b) => b -> Bool
{-# INLINE isReflection #-}
isReflection b = bxdfIs (bxdfType b) Reflection

isTransmission :: (Bxdf b) => b -> Bool
{-# INLINE isTransmission #-}
isTransmission b = bxdfIs (bxdfType b) Transmission

isSpecular :: (Bxdf b) => b -> Bool
{-# INLINE isSpecular #-}
isSpecular b = bxdfIs (bxdfType b) Specular

mkBxdfType :: [BxdfProp] -> BxdfType
{-# INLINE mkBxdfType #-}
mkBxdfType ps = BxdfType $ foldl' (\a p -> a .|. (fromBxdfProp p)) 0 ps

combineBxdfType :: BxdfType -> BxdfType -> BxdfType
{-# INLINE combineBxdfType #-}
combineBxdfType t1 t2 = BxdfType $ (unBxdfType t1 .|. unBxdfType t2)

-- | for @Diffuse@, @Glossy@ or @Specular@
bxdfAllTypes :: BxdfType
bxdfAllTypes = mkBxdfType [Diffuse, Glossy, Specular]

-- | for @Diffuse@, @Glossy@ or @Specular@ @Reflection@s
bxdfAllReflection :: BxdfType
bxdfAllReflection = combineBxdfType bxdfAllTypes $ mkBxdfType [Reflection]

-- | for @Diffuse@, @Glossy@ or @Specular@ @Transmission@s
bxdfAllTransmission :: BxdfType
bxdfAllTransmission = combineBxdfType bxdfAllTypes $ mkBxdfType [Transmission]

-- | for any kind of scattering
bxdfAll :: BxdfType
bxdfAll = combineBxdfType bxdfAllReflection bxdfAllTransmission

class Bxdf a where
   bxdfEval    :: a -> Normal -> Normal -> Spectrum
   bxdfSample  :: a -> Normal -> Rand2D -> (Spectrum, Normal, Flt)
   -- | samples the adjoint @Bxdf@
   bxdfSample' :: a -> Normal -> Rand2D -> (Spectrum, Normal, Flt)
   bxdfPdf     :: a -> Normal -> Normal -> Flt
   bxdfType    :: a -> BxdfType
   
   -- | has this @BxDF@ the provided flags set?
   bxdfMatches :: a -> BxdfType -> Bool
   
   bxdfSample a wo u = (f, wi, pdf) where
      wi = toSameHemisphere wo (cosineSampleHemisphere u)
      f = bxdfEval a wo wi
      pdf = bxdfPdf a wo wi

   bxdfSample' = bxdfSample
      
   bxdfPdf _ wo wi
      | sameHemisphere wo wi = invPi * absCosTheta wi
      | otherwise = 0

   bxdfMatches bxdf flags = bxdfIs' (bxdfType bxdf) flags

data AnyBxdf = forall a. Bxdf a => MkAnyBxdf a

instance Bxdf AnyBxdf where
   bxdfEval (MkAnyBxdf a) = bxdfEval a
   bxdfSample (MkAnyBxdf a) = bxdfSample a
   bxdfPdf (MkAnyBxdf a) = bxdfPdf a
   bxdfType (MkAnyBxdf a) = bxdfType a

--------------------------------------------------------------------------------
-- The BSDF (bidirectional scattering distribution function)
--------------------------------------------------------------------------------

data Bsdf = Bsdf
   { bsdfComponents  :: ! (V.Vector AnyBxdf) -- ^ the BxDFs the BSDF is composed of
   , _bsdfCs         :: {-# UNPACK #-} ! LocalCoordinates -- ^ the shading coordinate system
   , _bsdfP          :: {-# UNPACK #-} ! Point
   , _bsdfNg         :: {-# UNPACK #-} ! Normal -- ^ geometric normal
   }

-- | creates a BSDF
mkBsdf
   :: [AnyBxdf] -- ^ the BxDFs that constitute the BSDF
   -> DifferentialGeometry -- ^ the differential geometry for shading
   -> Normal -- ^ the normal from the geometry
   -> Bsdf
mkBsdf bs dgs ng = Bsdf (V.fromList bs) cs (dgP dgs) ng where
   cs = LocalCoordinates sn tn nn
   nn = dgN dgs
   sn = normalize $ dgDPDU dgs
   tn = nn `cross` sn

mkBsdf'
   :: [AnyBxdf]
   -> DifferentialGeometry -- ^ the geometric differential geometry
   -> DifferentialGeometry -- ^ the differential geometry for shading
   -> Bsdf
mkBsdf' bs dgg dgs = mkBsdf bs dgs (dgN dgg)

-- | extracts the effective shading normal from a BSDF
bsdfShadingNormal :: Bsdf -> Normal
{-# INLINE bsdfShadingNormal #-}
bsdfShadingNormal bsdf = n where
   (LocalCoordinates _ _ n) = _bsdfCs bsdf

bsdfShadingPoint :: Bsdf -> Point
{-# INLINE bsdfShadingPoint #-}
bsdfShadingPoint bsdf = _bsdfP bsdf

bsdfNumComponents :: BxdfType -> Bsdf -> Int
bsdfNumComponents t bsdf = V.sum $ V.map go $ bsdfComponents bsdf where
   go bxdf = if bxdfIs' t (bxdfType bxdf) then 1 else 0

-- | the number of specular BxDFs in a BSDF
bsdfSpecCompCount :: Bsdf -> Int
{-# INLINE bsdfSpecCompCount #-}
bsdfSpecCompCount = bsdfNumComponents (mkBxdfType [Specular])

-- | does the @BSDF@ contain non-specular components?
bsdfHasNonSpecular :: Bsdf -> Bool
{-# INLINE bsdfHasNonSpecular #-}
bsdfHasNonSpecular bsdf = V.or $ V.map (\x -> not $ isSpecular x) (bsdfComponents bsdf)

bsdfPdf :: Bsdf -> Vector -> Vector -> Float
bsdfPdf (Bsdf bs cs _ _) woW wiW 
   | V.null bs = 0
   | otherwise = {-# SCC "bsdfPdf" #-} pdfSum / fromIntegral (V.length bs) where
      pdfSum = V.sum $ V.map (\b -> bxdfPdf b wo wi) bs
      wo = worldToLocal cs woW
      wi = worldToLocal cs wiW

data BsdfSample = BsdfSample {
   bsdfSampleType       :: {-# UNPACK #-} ! BxdfType,
   bsdfSamplePdf        :: {-# UNPACK #-} ! Flt,
   bsdfSampleTransport  :: {-# UNPACK #-} ! Spectrum,
   bsdfSampleWi         :: {-# UNPACK #-} ! Vector
   } deriving (Show)

sampleBsdf :: Bsdf -> Vector -> Flt -> Rand2D -> BsdfSample
sampleBsdf = {-# SCC "sampleBsdf" #-} sampleBsdf' bxdfAll

sampleBsdf' :: BxdfType -> Bsdf -> Vector -> Flt -> Rand2D -> BsdfSample
sampleBsdf' = {-# SCC "sampleBsdf'" #-} sampleBsdf'' False

sampleAdjBsdf :: Bsdf -> Vector -> Flt -> Rand2D -> BsdfSample
sampleAdjBsdf = {-# SCC "sampleAdjBsdf" #-} sampleAdjBsdf' bxdfAll

sampleAdjBsdf' :: BxdfType -> Bsdf -> Vector -> Flt -> Rand2D -> BsdfSample
sampleAdjBsdf' = {-# SCC "sampleAdjBsdf'" #-} sampleBsdf'' True

sampleBsdf'' :: Bool -> BxdfType -> Bsdf -> Vector -> Flt -> Rand2D -> BsdfSample
{-# INLINE sampleBsdf'' #-}
sampleBsdf'' adj flags bsdf@(Bsdf bs cs _ ng) woW uComp uDir
   | cntm == 0 || pdf' == 0 || sideTest == 0 = emptyBsdfSample
   | isSpecular bxdf = BsdfSample t (pdf' / fromIntegral cntm) (sScale f' ff) wiW
   | otherwise = BsdfSample t pdf (sScale f ff) wiW where
      wo = worldToLocal cs woW
      
      -- choose BxDF to sample
      bsm = V.filter (\b -> bxdfMatches b flags) bs
      cntm = V.length bsm
      sNum = min (cntm-1) (floor (uComp * fromIntegral cntm)) -- index to sample
      bxdf = V.unsafeIndex bsm sNum

      -- sample chosen BxDF
      (f', wi, pdf') = let fun = if adj then bxdfSample' else bxdfSample
                       in fun bxdf wo uDir
      wiW = localToWorld cs wi

      -- overall PDF
      bs' = V.ifilter (\i _ -> (i /= sNum)) bsm -- filter explicitely sampled from matching
      pdf = if cntm == 1
               then pdf'
               else (pdf' + (V.sum $ V.map (\b -> bxdfPdf b wo wi) bs')) / (fromIntegral cntm)
      
      -- throughput for sampled direction
      sideTest = woW `dot` ng * wiW `dot` ng
      flt = if sideTest < 0 then isTransmission else isReflection
      f = V.sum $ V.map (\b -> bxdfEval b wo wi) $ V.filter flt bsm
      t = bxdfType bxdf
      
      -- correct throughput in presence of shading normals
      ns = bsdfShadingNormal bsdf
      ff = if adj then abs (ns `dot` woW / ng `dot` woW) else 1

      
evalBsdf :: Bool -> Bsdf -> Vector -> Vector -> Spectrum
evalBsdf adj bsdf@(Bsdf bxdfs cs _ ng) woW wiW
   | sideTest == 0 = black
   | otherwise = {-# SCC "evalBsdf" #-} sScale f ff
   where
      sideTest = woW `dot` ng * wiW `dot` ng
      flt = if sideTest < 0 then isTransmission else isReflection
      f = V.sum $ V.map (\b -> bxdfEval b wo wi) $ V.filter flt bxdfs
      wo = worldToLocal cs woW
      wi = worldToLocal cs wiW
      
      -- correct throughput in presence of shading normals
      ns = bsdfShadingNormal bsdf
      ff = if adj then abs (ns `dot` woW / ng `dot` woW) else 1

emptyBsdfSample :: BsdfSample
emptyBsdfSample = BsdfSample (mkBxdfType [Reflection, Diffuse]) 0 black (Vector 0 1 0)

blackBodyMaterial :: Material
blackBodyMaterial dgg dgs = mkBsdf [] dgs (dgN dgg)

data Lambertian = Lambertian {-# UNPACK #-} !Spectrum

instance Bxdf Lambertian where
   bxdfEval (Lambertian r) _ _ = sScale r invPi
   bxdfType _ = mkBxdfType [Reflection, Diffuse]

--------------------------------------------------------------------------------
-- The Oren Nayar BxDF
--------------------------------------------------------------------------------

data OrenNayar = MkOrenNayar
   {-# UNPACK #-} ! Spectrum -- ^ reflectance
   {-# UNPACK #-} ! Flt -- ^ the A parameter
   {-# UNPACK #-} ! Flt -- ^ the B parameter

mkOrenNayar
   :: Spectrum -- ^ the relfectance
   -> Flt -- ^ sigma, should be in [0..1] and is clamped otherwise
   -> OrenNayar

mkOrenNayar r sig = MkOrenNayar r a b where
   sig' = clamp sig 0 1
   sig2 = sig' * sig'
   a = 1 - (sig2 / (2 * (sig2 + 0.33)))
   b = 0.45 * sig2 / (sig2 + 0.09)

instance Bxdf OrenNayar where
   bxdfType _ = mkBxdfType [Reflection, Diffuse]
   
   bxdfEval (MkOrenNayar r a b) wo wi = r' where
      r' = sScale r (invPi * (a + b * maxcos * sina * tanb))
      (sina, tanb) = if absCosTheta wi > absCosTheta wo
                        then (sinto, sinti / absCosTheta wi)
                        else (sinti, sinto / absCosTheta wo)
      (sinti, sinto) = (sinTheta wi, sinTheta wo)
      maxcos
         | sinti > 1e-4 && sinto > 1e-4 =
            let
               (sinpi, cospi) = (sinPhi wi, cosPhi wi)
               (sinpo, cospo) = (sinPhi wo, cosPhi wo)
               dcos = cospi * cospo + sinpi * sinpo
            in max 0 dcos
         | otherwise = 0

--------------------------------------------------------------------------------
-- Microfacet Distributions
--------------------------------------------------------------------------------

data Microfacet = Microfacet {
   distribution   :: ! Distribution,
   fresnel        :: ! Fresnel,
   reflectance    :: {-# UNPACK #-} ! Spectrum
   }

instance Bxdf Microfacet where
   bxdfType _ = mkBxdfType [Reflection, Glossy]
   bxdfEval (Microfacet d fresn r) wo wi
      | costi == 0 || costo == 0 = black
      | vx wh' == 0 && vy wh' == 0 && vz wh' == 0 = black
      | otherwise = sScale (r * fresn costh) x
      where
         x = mfDistD d wh * mfG wo wi wh / (4 * costi * costo)
         costo = absCosTheta wo
         costi = absCosTheta wi
         wh' = wi + wo
         wh = normalize $ wh'
         costh = wi `dot` wh

   bxdfSample mf wo dirU = mfSample dirU mf wo

   bxdfPdf (Microfacet d _ _) wo wi
      | sameHemisphere wo wi = mfDistPdf d wo wi
      | otherwise = 0

mfSample :: Rand2D -> Microfacet -> Vector -> (Spectrum, Vector, Float)
mfSample dirU mf@(Microfacet d _ _) wo
   | sameHemisphere wo wi = (f, wi, pdf)
   | otherwise = (black, wo, 0)
   where
         f = bxdfEval mf wo wi
         (pdf, wi) = mfDistSample d dirU wo

mfG :: Vector -> Vector -> Vector -> Float
mfG wo wi wh = min 1 $ min
      (2 * nDotWh * nDotWo / woDotWh)
      (2 * nDotWh * nDotWi / woDotWh) where
         nDotWh = absCosTheta wh
         nDotWo = absCosTheta wo
         nDotWi = absCosTheta wi
         woDotWh = wo `absDot` wh

data Distribution
   = Blinn {-# UNPACK #-} !Flt -- ^ e
   | Anisotropic {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt -- ^ ex and ey

-- | fixes the exponent for microfacet distribution to a usable range
fixExponent :: Flt -> Flt
fixExponent e = if e > 10000 || isNaN e then 10000 else e

-- | create a Blinn microfacet distribution
mkBlinn
   :: Flt -- ^ the exponent in [0..10000]
   -> Distribution
mkBlinn e = Blinn $ fixExponent e

mkAnisotropic :: Flt -> Flt -> Distribution
mkAnisotropic ex ey = Anisotropic (fixExponent ex) (fixExponent ey)

mfDistPdf :: Distribution -> Vector -> Vector -> Float
mfDistPdf (Anisotropic ex ey) wo wi
   | ds > 0 && wo `dot` wh > 0 = d / (4 * (wo `dot` wh))
   | otherwise = 0
   where
      wh = normalize $ wo + wi
      costh = absCosTheta wh
      ds = 1 - costh * costh
      (whx, why) = (vx wh, vy wh)
      e = (ex * whx * whx + ey * why * why) / ds
      d = sqrt ((ex + 1) * (ey + 1)) * invTwoPi * (costh ** e)
      
mfDistPdf (Blinn e) wo wi = (e + 2) * (cost ** e) / (2 * pi * 4 * dot wo h) where
   h@(Vector _ _ hz) = normalize $ wo + wi
   cost = abs hz
   
mfDistSample :: Distribution -> Rand2D -> Vector -> (Float, Vector)

mfDistSample (Anisotropic ex ey) (u1, u2) wo
   | ds > 0 && wo `dot` wh > 0 = (d / (4 * wo `dot` wh), wi)
   | otherwise = (0, wi)
   where
      wi = -wo + (wh * (vpromote $ 2 * (wo `dot` wh)))
      wh' = sphericalDirection sint cost phi
      wh = if sameHemisphere wo wh' then wh' else -wh'
      sint = sqrt $ max 0 (1 - cost * cost)
      ds = 1 - cost * cost
      d = sqrt ((ex + 1) * (ey + 1)) * invTwoPi * (cost ** e)
      (whx, why) = (vx wh, vy wh)
      e = (ex * whx * whx + ey * why * why) / ds
      
      (phi, cost)
         | u1 < 0.25 = smpFirstQuadrand (4 * u1) id
         | u1 < 0.50 = smpFirstQuadrand (4 * (0.5 - u1)) (\x -> pi - x)
         | u1 < 0.75 = smpFirstQuadrand (4 * (u1 - 0.5)) (\x -> x + pi)
         | otherwise = smpFirstQuadrand (4 * (1 - u1)) (\x -> twoPi - x)
         
      smpFirstQuadrand u1' rsmp = (rsmp p, c) where
         p = if ex == ey
                then pi * u1' * 0.5
                else atan $ (sqrt $ (ex + 1) / (ey + 1)) * tan (pi * u1' * 0.5)
         (cp, sp) = (cos p, sin p)
         c = u2 ** (1 / (ex * cp * cp + ey * sp * sp + 1))
      
mfDistSample (Blinn e) (u1, u2) wo = (pdf, wi) where
   pdf = (e + 2) * (cost ** e) / (2 * pi * 4 * dot wo h) -- possible divide by zero?
   wi = (-wo) + (h * vpromote (2 * dot h wo))
   h = toSameHemisphere wo $ sphericalDirection sint cost phi
   cost = u1 ** (1 / (e + 1))
   sint = sqrt $ max 0 (1 - cost * cost)
   phi = u2 * 2 * pi

mfDistD :: Distribution -> Vector -> Float
mfDistD (Anisotropic ex ey) wh
   | d == 0 = 0
   | otherwise = sqrt ((ex + 2) * (ey + 2)) * invTwoPi * (costh ** e)
   where
      costh = absCosTheta wh
      d = 1 - costh * costh
      (whx, why) = (vx wh, vy wh)
      e = (ex * whx * whx + ey * why * why) / d
mfDistD (Blinn e) wh = (e + 2) * invTwoPi * (costh ** e) where
   costh = absCosTheta wh

--------------------------------------------------------------------------------
-- Bump Mapping
--------------------------------------------------------------------------------

bumpMapped :: ScalarTexture -> Material -> Material
bumpMapped d mat dgg dgs = mat dgg dgBump where
   dgBump = dgs { dgN = nn, dgDPDU = dpdu, dgDPDV = dpdv }
   
   uDisp = d dgeu
   vDisp = d dgev
   disp = d dgs
   
   vscale = vpromote $ (vDisp - disp) / dv
   dpdv = (dgDPDV dgs) + vscale * (dgN dgs) + (vpromote disp) * (dgDNDV dgs)
   
   uscale = vpromote $ (uDisp - disp) / du
   dpdu = (dgDPDU dgs) + uscale * (dgN dgs) + (vpromote disp) * (dgDNDU dgs)
   
   nn' = normalize $ dpdu `cross` dpdv
   nn = faceForward nn' $ dgN dgg -- match geometric normal
   
   -- shift in u
   du = 0.01 :: Flt
   vdu = vpromote du
   dgeu = dgs {
      dgP = dgP dgs + vdu * (dgDPDU dgs),
      dgU = dgU dgs + du,
      dgN = normalize $ ((dgDPDU dgs) `cross` (dgDPDV dgs) + vdu * (dgDNDU dgs))
      }
      
   -- shift in v
   dv = 0.01 :: Flt
   vdv = vpromote dv
   dgev = dgs {
      dgP = dgP dgs + vdv * (dgDPDV dgs),
      dgV = dgV dgs + dv,
      dgN = normalize $ ((dgDPDU dgs) `cross` (dgDPDV dgs) + vdv * (dgDNDV dgs))
      }
      