
module Graphics.Bling.Fresnel (

   -- * Fresnel incidence effects
   
   Fresnel, frDielectric, frConductor, frNoOp, frApproxEta, frApproxK
   
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Spectrum

type Fresnel = Float -> Spectrum

-- | a no-op Fresnel implementation, which always returns white
--   @Spectrum@
frNoOp :: Fresnel
frNoOp = const white

-- | Fresnel incidence effects for dielectrics
frDielectric :: Float -> Float -> Fresnel
{-
frDielectric _ etat cosi = frDiel' (abs cosi) cost (if cosi > 0 then sConst etat else sScale white (1 / etat))
   where
      cost = let c = sClamp 0 1 cost' in sqrt $ white - c
      cost' = let c = sConst $ max 0 (1 - cosi * cosi) in if cosi > 0
         then sScale c (1 / (etat * etat))
         else sScale c (etat * etat)
-}

frDielectric etai etat cosi = frDiel (abs cosi) cost (sConst etai) (sConst etat)
   where
      cost = let c = clamp cost' 0 1 in sqrt $ 1 - c
      cost' = let c = max 0 (1 - cosi * cosi) in if cosi > 0
         then c / (etat * etat)
         else c * (etat * etat)
         
frDiel
   :: Float    -- ^ cosi
   -> Float    -- ^ cost
   -> Spectrum -- ^ etai
   -> Spectrum -- ^ etat
   -> Spectrum -- ^ f
frDiel cosi cost etai etat = frDiel' cosi (sConst cost) (etat / etai)

frDiel'
   :: Float    -- ^ cosi
   -> Spectrum -- ^ cost
   -> Spectrum -- ^ eta
   -> Spectrum -- ^ f
frDiel' cosi cost eta = sScale (rParl * rParl + rPerp * rPerp) 0.5 where
   rParl'   = sScale eta cosi
   rParl    = (cost - rParl') / (cost + rParl')
   rPerp'   = eta * cost
   rPerp    = (sConst cosi - rPerp') / (sConst cosi + rPerp')

-- | Fresnel incidence effects for conductors
frConductor
   :: Spectrum -- ^ eta
   -> Spectrum -- ^ k
   -> Fresnel
frConductor eta k cosi = (rPer2 + rPar2) / 2 where
   rPer2 = (tmpF - ec2 + sConst (acosi * acosi)) /
           (tmpF + ec2 + sConst (acosi * acosi))
   rPar2 = (tmp - ec2 + white) /
           (tmp + ec2 + white)
   ec2 = sScale eta (2 * acosi)
   tmp = sScale (eta * eta + k * k) (acosi * acosi)
   tmpF = eta * eta + k * k
   acosi = abs cosi

frApproxEta :: Spectrum -> Spectrum
frApproxEta r = (white + r') / (white - r') where
   r' = sqrt $ sClamp 0 0.999 r

frApproxK :: Spectrum -> Spectrum
frApproxK r = sScale (sqrt $ refl / (white - refl)) 2 where
   refl = sClamp 0 0.999 r

