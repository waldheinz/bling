
module Graphics.Bling.Reflection.Specular (

   specRefl, specTrans
   
   ) where

import Graphics.Bling.Reflection

--   | SpecTrans    !Spectrum {-# UNPACK #-} !Float {-# UNPACK #-} !Float
--   | SpecRefl     !Spectrum !Fresnel   


specRefl
   :: Fresnel
   -> BxDF
specRefl fr = mkBxDF [Reflection, Specular] e s p where
   e _ _ = black
   p _ _ = 0
   
   s adj wo _ = (fr $ cosTheta wo, wi, 1) where
      wi = mkV (-(vx wo), -(vy wo), vz wo)

specTrans
   :: Spectrum    -- ^ transmitted spectrum
   -> Float       -- ^ eta incoming
   -> Float       -- ^ eta transmitted
   -> BxDF
specTrans = undefined

{-
bxdfSample (SpecTrans t ei et) wo u = sampleSpecTrans False t ei et wo u
bxdfSample (SpecRefl r fr) wo@(Vector x y z) _ = (f, wi, 1) where
      wi = Vector (-x) (-y) z
      f = sScale (r * fr (cosTheta wo)) (1 / absCosTheta wi)


bxdfSample' (SpecTrans t ei et) wo u = sampleSpecTrans True t ei et wo u
bxdfSample' sr@(SpecRefl _ _) wo u = bxdfSample sr wo u
bxdfSample' a wo u = {-# SCC "bxdfSample'" #-} (sScale f (absCosTheta wo / pdf), wi, pdf) where
      wi = toSameHemisphere wo (cosineSampleHemisphere u)
      f = bxdfEval a wo wi
      pdf = bxdfPdf a wo wi


bxdfType (SpecTrans _ _ _)    = 
bxdfType (SpecRefl _ _)       = mkBxdfType [Reflection, Specular]


--------------------------------------------------------------------------------
-- Specular Reflection / Transmission 
--------------------------------------------------------------------------------

sampleSpecTrans :: Bool -> Spectrum -> Float -> Float
   -> Vector -> (Float, Float) -> (Spectrum, Vector, Float)
sampleSpecTrans adj t ei' et' wo@(Vector wox woy _) _
   | sint2 >= 1 = (black, wo, 0) -- total internal reflection
   | otherwise = (f, wi, 1)
   where
      -- find out which eta is incident / transmitted
      entering = cosTheta wo > 0
      (ei, et) = if entering then (ei', et') else (et', ei')

      -- find transmitted ray direction
      sini2 = sinTheta2 wo
      eta = ei / et
      sint2 = eta * eta * sini2
      cost = let x =  sqrt $ max 0 (1 - sint2)
                in if entering then (-x) else x
      wi = mkV (eta * (-wox), eta * (-woy), cost)
      fr = frDielectric ei et $ cosTheta wo
      f' = (white - fr) * t
      f = if adj
            then sScale f' (1 / absCosTheta wi)
            else sScale f' (((et * et) / (ei * ei)) / absCosTheta wi)
-}
