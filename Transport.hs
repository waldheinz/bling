{-# LANGUAGE ExistentialQuantification #-}

module Transport(
   Bsdf, mkBsdf, BsdfSample(..), sampleBsdf, evalBsdf, bsdfPdf, filterBsdf,
   Bxdf(..), AnyBxdf(..), BxdfType, BxdfProp(..), mkBxdfType,
   isDiffuse, isReflection, sameHemisphere, toSameHemisphere, cosTheta, sinTheta2
   ) where

import Color
import Math
import Random

import Data.BitSet
import Data.List(foldl')
import qualified Data.Vector as V

data BxdfProp = Transmission | Reflection | Diffuse | Glossy | Specular deriving (Eq, Enum, Show)

type BxdfType = BitSet BxdfProp

-- | turns the second vector so it lies within the same hemisphere as
--   the first vector (assumed that both vectors are in shading coordinate
--   system)
toSameHemisphere :: Vector -> Vector -> Vector
toSameHemisphere (_, _, z1) (x, y, z2)
   | z1 * z2 >= 0 = (x, y, z2)
   | otherwise = (x, y, -z2)

-- | decides if two vectors in shading coordinate system lie within the
--   same hemisphere
sameHemisphere :: Vector -> Vector -> Bool
sameHemisphere (_, _, z1) (_, _, z2) = (z1 * z2 > 0)

cosTheta :: Vector -> Float
cosTheta (_, _, z) = z

sinTheta2 :: Vector -> Float
sinTheta2 v = 1 - (cosTheta v) * (cosTheta v)

isDiffuse :: (Bxdf b) => b -> Bool
isDiffuse b = Diffuse `member` (bxdfType b)

isReflection :: (Bxdf b) => b -> Bool
isReflection b = Reflection `member` (bxdfType b)

isTransmission :: (Bxdf b) => b -> Bool
isTransmission b = Transmission `member` (bxdfType b)

mkBxdfType :: [BxdfProp] -> BxdfType
mkBxdfType ps = foldl' (flip $ insert) empty ps

class Bxdf a where
   bxdfEval :: a -> Normal -> Normal -> Spectrum
   bxdfSample :: a -> Normal -> Rand2D -> (Spectrum, Normal, Float)
   bxdfPdf :: a -> Normal -> Normal -> Float
   bxdfType :: a -> BxdfType
   
   bxdfSample a wo u = (f, wi, pdf) where
      wi = cosineSampleHemisphere u
      f = bxdfEval a wo wi
      pdf = bxdfPdf a wo wi
      
   bxdfPdf _ (_, _, woz)(_, _, wiz)
      | woz * wiz > 0 = invPi * abs wiz
      | otherwise = 0

data AnyBxdf = forall a. Bxdf a => MkAnyBxdf a

instance Bxdf AnyBxdf where
   bxdfEval (MkAnyBxdf a) wo wi = bxdfEval a wo wi
   bxdfSample (MkAnyBxdf a) wo = bxdfSample a wo
   bxdfPdf (MkAnyBxdf a) wo wi = bxdfPdf a wo wi
   bxdfType (MkAnyBxdf a) = bxdfType a
   
data BsdfSample = BsdfSample {
   bsdfSampleType :: BxdfType,
   bsdfSamplePdf :: Float,
   bsdfSampleTransport :: Spectrum,
   bsdfSampleWi :: Vector
   } deriving (Show)

emptyBsdfSample :: BsdfSample
emptyBsdfSample = BsdfSample (mkBxdfType [Reflection, Diffuse]) 0 black (0,1,0)

-- | creates a Bsdf from a list of Bxdfs and a shading coordinate system
mkBsdf :: [AnyBxdf] -> LocalCoordinates -> Bsdf
mkBsdf bs cs = Bsdf (V.fromList bs) cs

data Bsdf = Bsdf (V.Vector AnyBxdf) LocalCoordinates

-- | filters a Bsdf's components by appearance
filterBsdf :: BxdfProp -> Bsdf -> Bsdf
filterBsdf ap (Bsdf bs cs) = Bsdf bs' cs where
   bs' = V.filter (\b -> member ap $ bxdfType b) bs

bsdfPdf :: Bsdf -> Vector -> Vector -> Float
bsdfPdf (Bsdf bs cs) woW wiW =
   if (V.null bs)
      then 0.0
      else V.foldl' (+) 0.0 $ V.map (\b -> bxdfPdf b wo wi) bs where
         wo = worldToLocal cs woW
         wi = worldToLocal cs wiW

sampleBsdf :: Bsdf -> Vector -> Float -> Rand2D -> BsdfSample
sampleBsdf (Bsdf bs cs) woW uComp uDir =
   if (V.null bs)
      then emptyBsdfSample
      else BsdfSample (bxdfType bxdf) pdf f wiW where
         f = V.foldl' (+) f' $ V.map (\b -> bxdfEval b wo wi) bs'
         pdf = (V.foldl' (+) pdf' $ V.map (\b -> bxdfPdf b wo wi) bs') / (fromIntegral bxdfCount + 1)
         bs' = V.ifilter (\ i _ -> (i /= sNum)) bs -- filter out explicitely sampled Bxdf
         (f', wi, pdf') = bxdfSample bxdf wo uDir
         wiW = localToWorld cs wi
         wo = worldToLocal cs woW
         bxdf = V.unsafeIndex bs sNum
         sNum = min (bxdfCount-1) (floor (uComp * fromIntegral bxdfCount)) -- index of Bxdf to sample
         bxdfCount = V.length bs

evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf (Bsdf bxdfs sc@(LocalCoordinates _ _ n)) woW wiW = 
   V.foldl' (+) black $ V.map (\b -> bxdfEval b wo wi) $ V.filter flt bxdfs
   where
         flt = if ((dot woW n) * (dot wiW n) < 0) then isTransmission else isReflection
         wo = worldToLocal sc woW
         wi = worldToLocal sc wiW
      
