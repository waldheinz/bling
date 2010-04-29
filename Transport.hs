{-# LANGUAGE ExistentialQuantification #-}

module Transport(
   Bsdf(..), BsdfSample(..), sampleBsdf, evalBsdf,
   Bxdf(..), AnyBxdf(..), BxdfSample(..), BxdfAppearance(..), BxdfType(..),
   isDiffuse, isReflection
   ) where

import Color
import Math
import Random

import Control.Monad
import Data.List

data BxdfAppearance = Diffuse | Glossy | Specular deriving Eq
data BxdfType = Transmission | Reflection deriving Eq

isDiffuse :: (Bxdf b) => b -> Bool
isDiffuse b = bxdfAppearance b == Diffuse

isReflection :: (Bxdf b) => b -> Bool
isReflection b = bxdfType b == Reflection

isTransmission :: (Bxdf b) => b -> Bool
isTransmission b = bxdfType b == Transmission

class Bxdf a where
   bxdfEval :: a -> Normal -> Normal -> Spectrum
   bxdfSample :: a -> Normal -> Rand BxdfSample
   bxdfPdf :: a -> Normal -> Normal -> Float
   bxdfType :: a -> BxdfType
   bxdfAppearance :: a -> BxdfAppearance
   
   bxdfSample a wo = do
      wi <- cosineSampleHemisphere
      return (BxdfSample (bxdfEval a wo wi) wi (bxdfPdf a wo wi))
   
   bxdfPdf _ (_, _, woz)(_, _, wiz)
      | woz * wiz > 0 = invPi * abs wiz
      | otherwise = infinity

data AnyBxdf = forall a. Bxdf a => MkAnyBxdf a

instance Bxdf AnyBxdf where
   bxdfEval (MkAnyBxdf a) wo wi = bxdfEval a wo wi
   bxdfSample (MkAnyBxdf a) wo = bxdfSample a wo
   bxdfPdf (MkAnyBxdf a) wo wi = bxdfPdf a wo wi
   bxdfType (MkAnyBxdf a) = bxdfType a
   bxdfAppearance (MkAnyBxdf a) = bxdfAppearance a
   
data BxdfSample = BxdfSample {
   bxdfSampleF :: Spectrum,
   bxdfSampleWi :: Normal,
   bxdfSamplePdf :: Float
   }
   
data BsdfSample = BsdfSample {
   bsdfSampleType :: BxdfType,
   bsdfSampleAppearance :: BxdfAppearance,
   bsdfSamplePdf :: Float,
   bsdfSampleTransport :: Spectrum,
   bsdfSampleWi :: Vector
   }

emptyBsdfSample :: BsdfSample
emptyBsdfSample = BsdfSample Reflection Diffuse infinity black (0,0,0)

data Bsdf = Bsdf [AnyBxdf] LocalCoordinates 
          | BlackbodyBsdf -- ^ a shortcut for blackbody emitters

sampleBsdf :: Bsdf -> Vector -> Rand BsdfSample
sampleBsdf BlackbodyBsdf _ = return emptyBsdfSample
sampleBsdf (Bsdf [] _ ) _ = return emptyBsdfSample
sampleBsdf (Bsdf bs cs) woW = do
   sNum <- rndRI (0, compCount - 1)
   b <- return $! (bs !! sNum)
   smp <- bxdfSample b wo
   return $! sampleBsdf' wo woW (bxdfType b) (bxdfAppearance b) smp (del sNum bs) cs
   where
         wo = worldToLocal cs woW
         compCount = length bs
         del n xs = a ++ drop 1 b where
            (a, b) = splitAt n xs
  
sampleBsdf' :: Vector -> Vector -> BxdfType -> BxdfAppearance -> BxdfSample -> [AnyBxdf] -> LocalCoordinates -> BsdfSample
sampleBsdf' _ _ bt Specular (BxdfSample f wi pdf) _ cs = BsdfSample bt Specular pdf f (localToWorld cs wi)
sampleBsdf' wo woW bt ba (BxdfSample f wi pdf) bs cs@(LocalCoordinates _ _ n) = BsdfSample bt ba pdf' f' wiW where
      pdf' = (foldl' (+) pdf $ map (\b -> bxdfPdf b wo wi) bs) / (fromIntegral $ length bs + 1)
      f' = foldl' (+) f $ map (\b -> bxdfEval b wo wi) (filter flt bs)
      flt = if ((dot woW n) * (dot wiW n) < 0) then isTransmission else isReflection
      wiW = localToWorld cs wi
      
evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf BlackbodyBsdf _ _ = black
evalBsdf (Bsdf bxdfs sc@(LocalCoordinates _ _ n)) woW wiW = 
   foldl' (+) black $ map (\b -> bxdfEval b wo wi) $ filter flt bxdfs
   where
         flt = if ((dot woW n) * (dot wiW n) < 0) then isTransmission else isReflection
         wo = worldToLocal sc woW
         wi = worldToLocal sc wiW
      
