{-# LANGUAGE ExistentialQuantification #-}

module Transport where

import Math
import Random

data BxdfType = Diffuse | Specular


class Bxdf a where
   bxdfEval :: a -> Normal -> Normal -> Spectrum
   bxdfSample :: a -> Normal -> Rand BxdfSample
   bxdfPdf :: a -> Normal -> Normal -> Float
   bxdfType :: a -> BxdfType
   
   bxdfSample a wo@(_, _, woz) = do
      wi' <- cosineSampleHemisphere
      return (BxdfSample (wif wi') (bxdfPdf a wo (wif wi'))) -- TODO: get rid of call to bxdfPdf
      where
            wif xx@(x, y, z)
               | woz < 0 = (x, y, -z)
               | otherwise = xx
   
   bxdfPdf _ wo wi@(_, _, z)
      | sameHemisphere wo wi = invPi * abs z
      | otherwise = 0

data AnyBxdf = forall a. Bxdf a => MkAnyBxdf a

instance Bxdf AnyBxdf where
   bxdfEval (MkAnyBxdf a) wo wi = bxdfEval a wo wi
   bxdfSample (MkAnyBxdf a) wo = bxdfSample a wo
   bxdfPdf (MkAnyBxdf a) wo wi = bxdfPdf a wo wi
   bxdfType (MkAnyBxdf a) = bxdfType a

data BxdfSample = BxdfSample {
   bxdfSampleWi :: Normal,
   bxdfSamplePdf :: Float
   }

-- | decides if two vectors which must be in the shading coordinate system
-- are in the same hemisphere
sameHemisphere :: Vector -> Vector -> Bool
sameHemisphere (_, _, z1) (_, _, z2) = z1 * z2 > 0
