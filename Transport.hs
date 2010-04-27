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

data BxdfAppearance = Diffuse | Glossy | Specular deriving Eq
data BxdfType = Transmission | Reflection deriving Eq

isDiffuse :: (Bxdf b) => b -> Bool
isDiffuse b = bxdfAppearance b == Diffuse

isReflection :: (Bxdf b) => b -> Bool
isReflection b = bxdfType b == Reflection

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
   bsdfsampleType :: BxdfType,
   bsdfSamplePdf :: Float,
   bsdfSampleTransport :: Spectrum,
   bsdfSampleWi :: Vector
   }

data Bsdf = Bsdf AnyBxdf LocalCoordinates

sampleBsdf :: Bsdf -> Vector -> Rand BsdfSample
sampleBsdf (Bsdf bxdf sc) woW = do
   (BxdfSample f wi pdf) <- bxdfSample bxdf wo
   return (BsdfSample bt pdf f (localToWorld sc wi))
      where
         bt = bxdfType bxdf
         wo = worldToLocal sc woW
         
evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf (Bsdf bxdf sc@(LocalCoordinates _ _ n)) woW wiW
   | isReflection bxdf && ((dot woW n) * (dot wiW n) > 0) = bxdfEval bxdf wo wi 
   | otherwise = black
   where
      wo = worldToLocal sc woW
      wi = worldToLocal sc wiW
      
