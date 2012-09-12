
module Graphics.Bling.Reflection.Diffuse (
   mkLambertian, mkOrenNayar,
   ) where

import Graphics.Bling.Montecarlo
import Graphics.Bling.Reflection

--   | OrenNayar    !Spectrum {-# UNPACK #-} !Float {-# UNPACK #-} !Float

cosPdf :: BxdfPdf
cosPdf wo wi
   | sameHemisphere wo wi = invPi * absCosTheta wi
   | otherwise = 0

cosSample :: Spectrum -> BxdfSample
cosSample r adj wo u
   | sameHemisphere wo wi = (rs, wi, cosPdf wo wi)
   | otherwise = (black, wo, 0)
   where
      wi = toSameHemisphere wo (cosineSampleHemisphere u)
      rs = if adj
            then sScale r (abs $ cosTheta wo / cosTheta wi)
            else r

mkLambertian :: Spectrum -> BxDF
mkLambertian r = mkBxDF (mkBxdfType [Reflection, Diffuse]) e (cosSample r) cosPdf where
   e wo _ = sScale r $ invPi * absCosTheta wo   -- eval
   
-- Creates an Oren Nayar BxDF
mkOrenNayar
   :: Spectrum -- ^ reflectance
   -> Float    -- ^ sigma, should be in [0..1] and is clamped otherwise
   -> BxDF
mkOrenNayar r sig = mkBxDF (mkBxdfType [Reflection, Diffuse]) e s cosPdf where
   sig2 = let sig' = clamp sig 0 1 in sig' * sig'
   a = 1 - (sig2 / (2 * (sig2 + 0.33)))
   b = 0.45 * sig2 / (sig2 + 0.09)
   
   s False wo u
      | sameHemisphere wo wi = (orenNayar wo wi, wi, cosPdf wo wi)
      | otherwise = (black, wi, 0)
      where
         wi = toSameHemisphere wo (cosineSampleHemisphere u)
         
   s True wo u
      | sameHemisphere wo wi = (r', wi, cosPdf wo wi)
      | otherwise = (black, wi, 0)
      where
         r' = sScale (orenNayar wo wi) (abs $ cosTheta wo / cosTheta wi) 
         wi = toSameHemisphere wo (cosineSampleHemisphere u)
      
   e wo wi = sScale (orenNayar wo wi) $ invPi * absCosTheta wo
      
   orenNayar wo wi = sScale r (a + b * maxcos * sina * tanb) where
      (sina, tanb) = if absCosTheta wi > absCosTheta wo
                        then (sinto, sinti / absCosTheta wi)
                        else (sinti, sinto / absCosTheta wo)
      (sinti, sinto) = (sinTheta wi, sinTheta wo)
   
      maxcos
         | sinti > 1e-4 && sinto > 1e-4 =
            let
               (sinpi, cospi) = (sinPhi wi, cosPhi wi)
               (sinpo, cospo) = (sinPhi wo, cosPhi wo)
            in max 0 $ cospi * cospo + sinpi * sinpo
         | otherwise = 0


