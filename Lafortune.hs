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
   
   bxdfType _ = mkBxdfType [Reflection, Glossy]
   
   bxdfEval (Lafortune diffuse lobes) (wox, woy, woz) (wix, wiy, wiz) =
      foldl (+) (sScale diffuse invPi) $ map evalLobe lobes where
         evalLobe (Lobe lX lY lZ lE) = sPow v lE where
            v = (sScale lX (wox * wix)) + (sScale lY (woy * wiy)) + (sScale lZ (woz * wiz)) 
      