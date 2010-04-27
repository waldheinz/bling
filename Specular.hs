
module Specular where

import Color

data SpecularReflection = SpecularReflection {
        
   }
        

type Fresnel = Float -> Spectrum
   
fresnelConductor :: Spectrum -> Spectrum -> Fresnel
fresnelConductor eta k cosi = (rPer2 + rPar2) / 2.0 where
   rPer2 = (tmpF - ec2 + sConst (cosi * cosi)) /
                (tmpF + ec2 + sConst (cosi * cosi))
   rPar2 = (tmp - ec2 + white) /
                (tmp + ec2 + white)
   ec2 = sScale eta (2 * cosi)
   tmp = sScale (eta * eta + k * k) (cosi * cosi)
   tmpF = eta * eta + k * k

