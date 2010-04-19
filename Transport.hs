{-# LANGUAGE ExistentialQuantification #-}

module Transport(
   Bxdf(..), AnyBxdf(..), BxdfSample(..), BxdfAppearance(..), BxdfType(..),
   isDiffuse, isReflection
   ) where

import Math
import Random

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
   
   bxdfSample a wo@(x, y, z) = do
      wi <- cosineSampleHemisphere
      return (BxdfSample wi (bxdfPdf a wo wi))
   
   bxdfPdf _ (_, _, woz)(_, _, wiz)
      | woz * wiz > 0 = invPi * abs wiz
      | otherwise = 0

data AnyBxdf = forall a. Bxdf a => MkAnyBxdf a

instance Bxdf AnyBxdf where
   bxdfEval (MkAnyBxdf a) wo wi = bxdfEval a wo wi
   bxdfSample (MkAnyBxdf a) wo = bxdfSample a wo
   bxdfPdf (MkAnyBxdf a) wo wi = bxdfPdf a wo wi
   bxdfType (MkAnyBxdf a) = bxdfType a
   bxdfAppearance (MkAnyBxdf a) = bxdfAppearance a
   
data BxdfSample = BxdfSample {
   bxdfSampleWi :: Normal,
   bxdfSamplePdf :: Float
   }
