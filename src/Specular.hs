
module Specular where

import Material
import Math
import Spectrum
import Transport

glassMaterial :: Float -> Spectrum -> Material
glassMaterial ior r dg = mkBsdf [refl, trans] cs where
   refl = MkAnyBxdf $ SpecularReflection r $ frDiel 1 ior
   trans = MkAnyBxdf $ SpecularTransmission r 1 ior
   cs = shadingCs dg

mirrorMaterial :: Spectrum -> Material
mirrorMaterial r dg = mkBsdf [bxdf] cs where
   bxdf = MkAnyBxdf $ SpecularReflection r frNoOp
   cs = coordinateSystem $ dgN dg

data SpecularTransmission = SpecularTransmission {
   specTransT :: Spectrum,
   specTransEi :: Float,
   specTransEt :: Float
   }

instance Bxdf SpecularTransmission where
   bxdfType _ = mkBxdfType [Transmission, Specular]
   bxdfEval _ _ _ = black
   bxdfPdf _ _ _ = 0
   bxdfSample (SpecularTransmission t ei' et') wo@(MkVector wox woy _) _
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
               wi = MkVector (eta * (-wox)) (eta * (-woy)) cost
               f' = frDiel ei et $ cosTheta wo
               f = sScale (t * (white - f')) (((et*et) / (ei*ei)) / abs (cosTheta wi))

data SpecularReflection = SpecularReflection {
   specReflR :: Spectrum,
   specReflFresnel :: Fresnel
   }
        
instance Bxdf SpecularReflection where
   bxdfType _ = mkBxdfType [Reflection, Specular]
   bxdfEval _ _ _ = black
   bxdfPdf _ _ _ = 0
   bxdfSample (SpecularReflection r fresnel) (MkVector x y z) _ = (f, wi, 1) where
      f = sScale (r * fresnel z) (1.0 / abs z)
      wi = MkVector (-x) (-y) z
           
type Fresnel = Float -> Spectrum

frNoOp :: Fresnel
frNoOp _ = white

frDiel :: Float -> Float -> Fresnel
frDiel ei et cosi
   | sint > 1 = white -- total internal reflection
   | otherwise = frDiel' (abs cosi') cost (sConst ei') (sConst et')
   where
      cost = sqrt $ max 0 (1 - sint * sint)
      sint = (ei' / et') * sqrt (max 0 (1 - cosi' * cosi'))
      cosi' = min 1 $ max (-1) cosi
      ei' = if cosi > 0 then ei else et
      et' = if cosi > 0 then et else ei

frDiel' :: Float -> Float -> Spectrum -> Spectrum -> Spectrum
frDiel' cosi cost etai etat = (rPar * rPar + rPer * rPer) / 2.0 where
   rPar = (sScale etat cosi - sScale etai cost) /
          (sScale etat cosi + sScale etai cost)
   rPer = (sScale etai cosi - sScale etat cost) /
          (sScale etai cosi + sScale etat cost)

frCond :: Spectrum -> Spectrum -> Fresnel
frCond eta k cosi = (rPer2 + rPar2) / 2.0 where
   rPer2 = (tmpF - ec2 + sConst (cosi * cosi)) /
                (tmpF + ec2 + sConst (cosi * cosi))
   rPar2 = (tmp - ec2 + white) /
                (tmp + ec2 + white)
   ec2 = sScale eta (2 * cosi)
   tmp = sScale (eta * eta + k * k) (cosi * cosi)
   tmpF = eta * eta + k * k
