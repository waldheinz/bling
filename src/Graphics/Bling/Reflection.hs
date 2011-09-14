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
   
   AnyBxdf(..), Bxdf(..), BxdfProp(..), BxdfType, mkBxdfType,
   Lambertian(..), mkOrenNayar, OrenNayar,
   
   -- * BSDF
   
   Bsdf, BsdfSample(..),
   mkBsdf, mkBsdf', evalBsdf, sampleBsdf, bsdfPdf,
   
   -- * Working with Vectors in shading coordinate system

   cosTheta, absCosTheta, sinTheta2, sinTheta, cosPhi, sinPhi,
   sameHemisphere
   
   ) where

import Data.BitSet
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

-- | a no-or Fresnel implementation, which always returns a fully white
--   @Spectrum@
frNoOp :: Fresnel
frNoOp _ = white

-- | Fresnel incidence effects for dielectrics
frDielectric :: Flt -> Flt -> Fresnel
frDielectric etai etat cosi
   | sint >= 1 = white -- total internal reflection
   | otherwise = frDiel' (abs cosi') cost (sConst ei) (sConst et)
   where
      cost = sqrt $ max 0 (1 - sint * sint)
      sint = (ei / et) * sqrt (max 0 (1 - cosi' * cosi'))
      cosi' = clamp cosi (-1) 1
      (ei, et) = if cosi' > 0 then (etai, etat) else (etat, etai)
   
frDiel' :: Flt -> Flt -> Spectrum -> Spectrum -> Spectrum
frDiel' cosi cost etai etat = (rPar * rPar + rPer * rPer) / 2 where
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
cosTheta = vz

absCosTheta :: Vector -> Flt
absCosTheta = abs . cosTheta

sinTheta2 :: Vector -> Flt
sinTheta2 v = max 0 (1 - cosTheta v * cosTheta v)

sinTheta :: Vector -> Flt
sinTheta = sqrt . sinTheta2

cosPhi :: Vector -> Flt
cosPhi v
   | sint == 0 = 1
   | otherwise = clamp (vx v / sint) (-1) 1
   where
         sint = sinTheta v

sinPhi :: Vector -> Flt
sinPhi v
   | sint == 0 = 0
   | otherwise = clamp (vy v / sint) (-1) 1
   where
         sint = sinTheta v


-- | decides if two vectors in shading coordinate system lie within the
--   same hemisphere
sameHemisphere :: Vector -> Vector -> Bool
sameHemisphere (Vector _ _ z1) (Vector _ _ z2) = z1 * z2 > 0

data BxdfProp = Transmission | Reflection | Diffuse | Glossy | Specular deriving (Eq, Enum, Show)

type BxdfType = BitSet BxdfProp

toSameHemisphere :: Vector -> Vector -> Vector
toSameHemisphere wo wi = if vz wo < 0 then wi {vz = -(vz wi)} else wi

class Bxdf a where
   bxdfEval :: a -> Normal -> Normal -> Spectrum
   bxdfSample :: a -> Normal -> Rand2D -> (Spectrum, Normal, Flt)
   bxdfPdf :: a -> Normal -> Normal -> Flt
   bxdfType :: a -> BxdfType

   bxdfSample a wo u = (f, wi, pdf) where
      wi = toSameHemisphere wo (cosineSampleHemisphere u)
      f = bxdfEval a wo wi
      pdf = bxdfPdf a wo wi
      
   bxdfPdf _ wo wi
      | sameHemisphere wo wi = invPi * absCosTheta wi
      | otherwise = 0

data AnyBxdf = forall a. Bxdf a => MkAnyBxdf a

instance Bxdf AnyBxdf where
   bxdfEval (MkAnyBxdf a) = bxdfEval a
   bxdfSample (MkAnyBxdf a) = bxdfSample a
   bxdfPdf (MkAnyBxdf a) = bxdfPdf a
   bxdfType (MkAnyBxdf a) = bxdfType a

isReflection :: (Bxdf b) => b -> Bool
isReflection b = Reflection `member` bxdfType b

isTransmission :: (Bxdf b) => b -> Bool
isTransmission b = Transmission `member` bxdfType b

isSpecular :: (Bxdf b) => b -> Bool
isSpecular b = Specular `member` bxdfType b

mkBxdfType :: [BxdfProp] -> BxdfType
mkBxdfType = foldl' (flip insert) empty

--------------------------------------------------------------------------------
-- The BSDF (bidirectional scattering distribution function)
--------------------------------------------------------------------------------

data Bsdf = Bsdf
   { _bsdfBxdfs :: V.Vector AnyBxdf -- ^ the BxDFs the BSDF is composed of
   , _bsdfCs    :: LocalCoordinates -- ^ the shading coordinate system
   , _bsdfNg    :: Normal -- ^ geometric normal
   }

-- | creates a BSDF
mkBsdf
   :: [AnyBxdf] -- ^ the BxDFs that constitute the BSDF
   -> DifferentialGeometry -- ^ the differential geometry for shading
   -> Normal -- ^ the normal from the geometry
   -> Bsdf
mkBsdf bs dg ng = Bsdf (V.fromList bs) cs ng where
   cs = LocalCoordinates sn tn nn
   nn = dgN dg
   sn = normalize $ dgDPDU dg
   tn = nn `cross` sn

mkBsdf'
   :: [AnyBxdf]
   -> DifferentialGeometry -- ^ the geometric differential geometry
   -> DifferentialGeometry -- ^ the differential geometry for shading
   -> Bsdf
mkBsdf' bs dgg dgs = mkBsdf bs dgs (dgN dgg)

data BsdfSample = BsdfSample {
   bsdfSampleType :: BxdfType,
   bsdfSamplePdf :: Float,
   bsdfSampleTransport :: Spectrum,
   bsdfSampleWi :: Vector
   } deriving (Show)

bsdfPdf :: Bsdf -> Vector -> Vector -> Float
bsdfPdf (Bsdf bs cs _) woW wiW
   | V.null bs = 0
   | otherwise = pdfSum / fromIntegral (V.length bs) where
      pdfSum = V.sum $ V.map (\b -> bxdfPdf b wo wi) bs
      wo = worldToLocal cs woW
      wi = worldToLocal cs wiW
      
sampleBsdf :: Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
sampleBsdf (Bsdf bs cs ng) woW uComp uDir
   | V.null bs || pdf' == 0 = emptyBsdfSample
   | isSpecular bxdf = BsdfSample t pdf' f' wiW
   | otherwise = BsdfSample t pdf f wiW where
      (f', wi, pdf') = bxdfSample bxdf wo uDir
      flt = if woW `dot` ng * wiW `dot` ng < 0 then isTransmission else isReflection
      f = f' + (V.sum $ V.map (\b -> bxdfEval b wo wi) $ V.filter flt bs')
      pdf = (pdf' + (V.sum $ V.map (\b -> bxdfPdf b wo wi) bs')) / (fromIntegral cnt)
      bs' = V.ifilter (\i _ -> (i /= sNum)) bs -- filter explicitely sampled
      wiW = localToWorld cs wi
      wo = worldToLocal cs woW
      bxdf = V.unsafeIndex bs sNum
      sNum = min (cnt-1) (floor (uComp * fromIntegral cnt)) -- index to sample
      cnt = V.length bs
      t = bxdfType bxdf
      
evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf (Bsdf bxdfs cs ng) woW wiW =
   V.sum $ V.map (\b -> bxdfEval b wo wi) $ V.filter flt bxdfs
   where
      flt = if dot woW ng * dot wiW ng < 0 then isTransmission else isReflection
      wo = worldToLocal cs woW
      wi = worldToLocal cs wiW

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
   Spectrum -- ^ reflectance
   Flt -- ^ the A parameter
   Flt -- ^ the B parameter

mkOrenNayar
   :: Spectrum -- ^ the relfectance
   -> Flt -- ^ the sigma parameter
   -> OrenNayar

mkOrenNayar r sig = MkOrenNayar r a b where
   a = 1 - (sig2 / (2 * (sig2 + 0.33)))
   b = 0.45 * sig2 / (sig2 + 0.09)
   sig2 = sig' * sig'
   sig' = radians sig

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
   distribution :: Distribution,
   fresnel :: Fresnel,
   reflectance :: Spectrum
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
      