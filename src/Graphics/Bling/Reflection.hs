
module Graphics.Bling.Reflection (
   module Graphics.Bling.DifferentialGeometry,
   module Graphics.Bling.Spectrum,
   
   -- * Materials and Bump Mapping

   Material, blackBodyMaterial, bump, bumpMapped,
   
   -- * BxDF Functions
   
   BxdfPdf, BxdfSample, BxDF, mkBxDF, brdfToBtdf,
   
   -- ** BxDF Types
   BxdfProp(..), BxdfType, bxdfIs, mkBxdfType,
   
   -- ** Working in shading coordinates
   
   cosTheta, absCosTheta, sinTheta, sinTheta2, sinPhi, cosPhi,
   sameHemisphere, toSameHemisphere,
   
   -- * BSDF
   
   Bsdf, BsdfSample(..), mkBsdf, mkBsdf', evalBsdf, sampleBsdf, sampleBsdf',
   bsdfPdf, sampleAdjBsdf, sampleAdjBsdf', bsdfNg,

   -- ** Querying BSDF properties
   bsdfHasNonSpecular, bsdfShadingNormal, bsdfShadingPoint, bsdfSpecCompCount
   
   ) where

import Data.Bits
import Data.List (foldl')
import qualified Data.Vector as V

import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Random
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture

-- | a material can turn a geometric DG and a shading DG into a BSDF
type Material = DifferentialGeometry -> DifferentialGeometry -> Bsdf

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

cosTheta :: Vector -> Float
{-# INLINE cosTheta #-}
cosTheta = vz

absCosTheta :: Vector -> Float
{-# INLINE absCosTheta #-}
absCosTheta = abs . cosTheta

sinTheta2 :: Vector -> Float
{-# INLINE sinTheta2 #-}
sinTheta2 v = max 0 (1 - cosTheta v * cosTheta v)

sinTheta :: Vector -> Float
{-# INLINE sinTheta #-}
sinTheta = sqrt . sinTheta2

cosPhi :: Vector -> Float
{-# INLINE cosPhi #-}
cosPhi v
   | sint == 0 = 1
   | otherwise = clamp (vx v / sint) (-1) 1
   where
         sint = sinTheta v

sinPhi :: Vector -> Float
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
-- BxDF Types
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

newtype BxdfType = BxdfType { unBxdfType :: Int }

instance Show BxdfType where
   show t = show $ bxdfTypeToProps t

bxdfIs :: BxdfType -> BxdfProp -> Bool
{-# INLINE bxdfIs #-}
bxdfIs b p = bxdfIs' (BxdfType $ fromBxdfProp p) b

bxdfIs' :: BxdfType -> BxdfType -> Bool
{-# INLINE bxdfIs' #-}
bxdfIs' t1 t2 = ((unBxdfType t1) .&. (unBxdfType t2)) == (unBxdfType t1)

isReflection :: BxDF -> Bool
{-# INLINE isReflection #-}
isReflection b = bxdfIs (bxdfType b) Reflection

isTransmission :: BxDF -> Bool
{-# INLINE isTransmission #-}
isTransmission b = bxdfIs (bxdfType b) Transmission

isSpecular :: BxDF -> Bool
{-# INLINE isSpecular #-}
isSpecular b = bxdfIs (bxdfType b) Specular

mkBxdfType :: [BxdfProp] -> BxdfType
{-# INLINE mkBxdfType #-}
mkBxdfType ps = BxdfType $ foldl' (\a p -> a .|. (fromBxdfProp p)) 0 ps

bxdfTypeToProps :: BxdfType -> [BxdfProp]
bxdfTypeToProps t = filter (bxdfIs t)
   [Reflection, Transmission, Diffuse, Glossy, Specular]

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

bxdfTypeFlip :: BxdfType -> BxdfType -> BxdfType
bxdfTypeFlip t f = BxdfType $ (unBxdfType t `xor` unBxdfType f)

--------------------------------------------------------------------------------
-- BxDF
--------------------------------------------------------------------------------

type BxdfEval     = Vector -> Vector -> Spectrum
type BxdfSample   = Bool -> Vector -> Rand2D -> (Spectrum, Vector, Float)
type BxdfPdf      = Vector -> Vector -> Float

data BxDF = BxDF
   { bxdfType     :: {-# UNPACK #-} !BxdfType
   , bxdfEval     :: ! BxdfEval
   , bxdfSample   :: ! BxdfSample
   , bxdfPdf      :: ! BxdfPdf
   }

mkBxDF :: [BxdfProp] -> BxdfEval -> BxdfSample -> BxdfPdf -> BxDF
mkBxDF ps e s p = BxDF (mkBxdfType ps) e s p

-- | has this @BxDF@ the provided flags set?
bxdfMatches :: BxDF -> BxdfType -> Bool
bxdfMatches bxdf flags = bxdfIs' (bxdfType bxdf) flags

brdfToBtdf :: BxDF -> BxDF
brdfToBtdf brdf = BxDF tp e s p where
   otherHemisphere w = mkV (vx w, vy w, -(vz w))
   e wo wi = bxdfEval brdf wo $ otherHemisphere wi
   s adj wo u = (f, otherHemisphere wi, pdf) where
      (f, wi, pdf) = bxdfSample brdf adj wo u
   p wo wi = bxdfPdf brdf wo $ otherHemisphere wi
   tp = bxdfTypeFlip (bxdfType brdf) (mkBxdfType [Reflection, Transmission])
   
--------------------------------------------------------------------------------
-- The BSDF (bidirectional scattering distribution function)
--------------------------------------------------------------------------------

data Bsdf = Bsdf
   { bsdfComponents  :: ! (V.Vector BxDF) -- ^ BxDFs the BSDF is composed of
   , _bsdfCs         :: {-# UNPACK #-} ! LocalCoordinates -- ^ shading coordinate system
   , _bsdfP          :: {-# UNPACK #-} ! Point
   , bsdfNg          :: {-# UNPACK #-} ! Normal -- ^ geometric normal
   }

-- | creates a BSDF
mkBsdf
   :: [BxDF] -- ^ the BxDFs that constitute the BSDF
   -> DifferentialGeometry -- ^ the differential geometry for shading
   -> Normal -- ^ the normal from the geometry
   -> Bsdf
mkBsdf bs dgs ng = Bsdf (V.fromList bs) cs (dgP dgs) ng where
   cs = LocalCoordinates sn tn nn
   nn = dgN dgs
   sn = normalize $ dgDPDU dgs
   tn = nn `cross` sn

mkBsdf'
   :: [BxDF]
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
   bsdfSamplePdf        :: {-# UNPACK #-} ! Float,
   bsdfSampleTransport  :: ! Spectrum,
   bsdfSampleWi         :: ! Vector
   } deriving (Show)

sampleBsdf :: Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
sampleBsdf = {-# SCC "sampleBsdf" #-} sampleBsdf' bxdfAll

sampleBsdf' :: BxdfType -> Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
sampleBsdf' = {-# SCC "sampleBsdf'" #-} sampleBsdf'' False

sampleAdjBsdf :: Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
sampleAdjBsdf = {-# SCC "sampleAdjBsdf" #-} sampleBsdf'' True bxdfAll

sampleAdjBsdf' :: BxdfType -> Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
sampleAdjBsdf' = {-# SCC "sampleAdjBsdf'" #-} sampleBsdf'' True

sampleBsdf'' :: Bool -> BxdfType -> Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
{-# INLINE sampleBsdf'' #-}
sampleBsdf'' adj flags (Bsdf bs cs _ ng) woW uComp uDir
   | V.null bsm || pdf' == 0 || sideTest == 0 = emptyBsdfSample
   | not (flt bxdf) = emptyBsdfSample
   | isSpecular bxdf =
      BsdfSample t (pdf' * invCnt) (sScale (fAdj fSample) cntf) wiW
   | cntm == 1 = BsdfSample t pdf' (fAdj fSample) wiW
   | otherwise = BsdfSample t pdf (fAdj fSum) wiW
   where
      wo = worldToLocal cs woW
      
      -- choose BxDF to sample
      bsm = V.filter (\b -> bxdfMatches b flags) bs
      cntm = V.length bsm
      cntf = fromIntegral cntm
      invCnt = 1 / cntf
      sNum = max 0 $ min (cntm - 1) (floor (uComp * cntf)) -- index to sample
      bxdf = V.unsafeIndex bsm sNum
      
      -- sample chosen BxDF
      (fSample, wi, pdf') = bxdfSample bxdf adj wo uDir
      wiW = localToWorld cs wi
      sideTest = wiW `dot` ng / woW `dot` ng
      flt = if sideTest < 0 then isTransmission else isReflection
      
      -- overall PDF
      bs' = V.ifilter (\i _ -> (i /= sNum)) bsm -- filter explicitly sampled
      pdf = (pdf' + (V.sum $ V.map (\b -> bxdfPdf b wo wi) bs')) * invCnt
      
      -- throughput for sampled direction
      fOthers = V.sum $ V.map (\b -> eval b wo wi) $ V.filter flt bs'
      eval b = if adj then bxdfEval b else flip (bxdfEval b)
      
      fSum = sScale (sScale fSample pdf' + fOthers) (1 / pdf)
      t = bxdfType bxdf
      
      -- correct throughput for adjoint
      fAdj ff = if adj then sScale ff (abs sideTest) else ff

evalBsdf :: Bool -> Bsdf -> Vector -> Vector -> Spectrum
evalBsdf adj (Bsdf bxdfs cs _ ng) woW wiW
   | sideTest == 0 = black
   | abs cosWo < 1e-5 = black -- filter NaN / Infinite results
   | adj = sScale f $ abs sideTest -- correct throughput for shading normals
   | otherwise = f
   where
      cosWo = woW `dot` ng
      sideTest = wiW `dot` ng / cosWo
      flt = if sideTest < 0 then isTransmission else isReflection
      f = V.sum $ V.map (\b -> eval b wo wi) $ V.filter flt bxdfs
      eval b = if adj then bxdfEval b else flip (bxdfEval b)
      wo = worldToLocal cs woW
      wi = worldToLocal cs wiW

emptyBsdfSample :: BsdfSample
emptyBsdfSample = BsdfSample (mkBxdfType [Reflection, Diffuse]) 0 black (Vector 0 1 0)

blackBodyMaterial :: Material
blackBodyMaterial dgg dgs = mkBsdf [] dgs (dgN dgg)

--------------------------------------------------------------------------------
-- Bump Mapping
--------------------------------------------------------------------------------

bumpMapped :: ScalarTexture -> Material -> Material
bumpMapped d mat dgg dgs = mat dgg $ bump d dgg dgs

bump :: ScalarTexture -> DifferentialGeometry -> DifferentialGeometry -> DifferentialGeometry
bump d dgg dgs = {-# SCC "bump" #-} dgBump where
   dgBump = dgs { dgN = nn , dgDPDU = dpdu, dgDPDV = dpdv }
   uDisp = d dgeu
   vDisp = d dgev
   disp = d dgs
   
   vscale = (vDisp - disp) / dv
   dpdv = (dgDPDV dgs) + vscale *# (dgN dgs) -- + (vpromote disp) * (dgDNDV dgs)
   
   uscale = (uDisp - disp) / du
   dpdu = (dgDPDU dgs) + uscale *# (dgN dgs) -- + (vpromote disp) * (dgDNDU dgs)
   
   nn' = normalize $ dpdu `cross` dpdv
   nn = faceForward nn' $ dgN dgg -- match geometric normal

   -- shift in u
   du = 0.01 :: Float
   dgeu = dgs {
      dgP = dgP dgs + du *# (dgDPDU dgs),
      dgU = dgU dgs + du,
      dgN = normalize $ ((dgDPDU dgs) `cross` (dgDPDV dgs) + du *# (dgDNDU dgs))
      }
      
   -- shift in v
   dv = 0.01 :: Float
   dgev = dgs {
      dgP = dgP dgs + dv *# (dgDPDV dgs),
      dgV = dgV dgs + dv,
      dgN = normalize $ ((dgDPDU dgs) `cross` (dgDPDV dgs) + dv *# (dgDNDV dgs))
      }
      
