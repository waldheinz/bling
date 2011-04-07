
module Graphics.Bling.Reflection (
   
   -- * Fresnel Equations
   
   Fresnel, frDiel, frCond, frNoOp
   
   ) where


import Graphics.Bling.Spectrum

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
