module Lafortune where

import Color
import Math
import Transport

data Lobe = Lobe {
   lobeX :: Spectrum,
   lobeY :: Spectrum,
   lobeZ :: Spectrum,
   lobeE :: Spectrum
   }
   
data Lafortune = Lafortune Spectrum [Lobe]

instance Bxdf Lafortune where
   
   bxdfType _ = Reflection
   bxdfAppearance _ = Glossy
   
   bxdfEval (Lafortune diffuse lobes) (wox, woy, woz) (wix, wiy, wiz) =
      foldl add (scalMul diffuse invPi) $ map evalLobe lobes where
         evalLobe (Lobe lX lY lZ lE) = pow v lE where
            v = (scalMul lX (wox * wix)) `add` (scalMul lY (woy * wiy)) `add` (scalMul lZ (woz * wiz)) 
      