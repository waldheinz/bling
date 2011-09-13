
module Graphics.Bling.Material.Specular (
   
   -- * Specular materials
   
   glassMaterial, mirrorMaterial,

   -- * Specular BxDFs
   
   SpecularReflection, SpecularTransmission,
   mkSpecRefl
   
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Reflection
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture

-- | a glass material
glassMaterial
   :: ScalarTexture -- ^ index of refraction
   -> SpectrumTexture -- ^ reflection color
   -> SpectrumTexture -- ^ transmission color
   -> Material
glassMaterial iort rt tt dgg dgs = mkBsdf' [refl, trans] dgg dgs where
   refl = MkAnyBxdf $ SpecularReflection r $ frDielectric 1 ior
   trans = MkAnyBxdf $ SpecularTransmission t 1 ior
   r = rt dgs
   t = tt dgs
   ior = iort dgs
   
mirrorMaterial
   :: SpectrumTexture -- ^ reflection color
   -> Material
mirrorMaterial rt dgg dgs = mkBsdf' [bxdf] dgg dgs where
   bxdf = MkAnyBxdf $ SpecularReflection r frNoOp
   r = sClamp 0 1 $ rt dgs

data SpecularTransmission = SpecularTransmission {
   _specTransT :: {-# UNPACK #-} !Spectrum,
   _specTransEi :: {-# UNPACK #-} !Float,
   _specTransEt :: {-# UNPACK #-} !Float
   }

instance Bxdf SpecularTransmission where
   bxdfType _ = mkBxdfType [Transmission, Specular]
   bxdfEval _ _ _ = black
   bxdfPdf _ _ _ = 0
   bxdfSample (SpecularTransmission t ei' et') wo@(Vector wox woy _) _
      | sint2 > 1 = (black, wo, 0) -- total internal reflection
      | otherwise = (f, wi, 1.0)
         where
               entering = cosTheta wo > 0
               sint2 = eta * eta * sini2
               eta = ei / et
               (ei, et) = if entering then (ei', et') else (et', ei')
               cost' = sqrt $ max 0 (1 - sint2)
               cost = if entering then (-cost') else cost'
               sini2 = sinTheta2 wo
               wi = Vector (eta * (-wox)) (eta * (-woy)) cost
               f' = frDielectric ei et $ cosTheta wo
               f = sScale (t * (white - f')) (((et*et) / (ei*ei)) / abs (cosTheta wi))

data SpecularReflection = SpecularReflection {
   _specReflR :: {-# UNPACK #-} !Spectrum,
   _specReflFresnel :: !Fresnel
   }

mkSpecRefl
   :: Spectrum 
   -> Fresnel
   -> SpecularReflection
   
mkSpecRefl = SpecularReflection

instance Bxdf SpecularReflection where
   bxdfType _ = mkBxdfType [Reflection, Specular]
   bxdfEval _ _ _ = black
   bxdfPdf _ _ _ = 0
   bxdfSample (SpecularReflection r fr) (Vector x y z) _ = (f, wi, 1) where
      f = sScale (r * fr z) (1.0 / abs z)
      wi = Vector (-x) (-y) z
