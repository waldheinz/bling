
module Graphics.Bling.Reflection.Specular (

   specRefl, specTrans
   
   ) where

import Graphics.Bling.Fresnel
import Graphics.Bling.Reflection

specRefl
   :: Fresnel
   -> Spectrum -- ^ reflected spectrum
   -> BxDF
specRefl fr r = mkBxDF [Reflection, Specular] e s p where
   e _ _ = black
   p _ _ = 0
   
   s _ wo _ = (r * (fr $ cosTheta wo), wi, 1) where
      wi = mkV (-(vx wo), -(vy wo), vz wo)

specTrans
   :: Spectrum    -- ^ transmitted spectrum
   -> Float       -- ^ eta incoming
   -> Float       -- ^ eta transmitted
   -> BxDF
specTrans t ei et = mkBxDF [Transmission, Specular] e s p where
   e _ _ = black
   p _ _ = 0
   
   s adj wo u = sampleSpecTrans adj t ei et wo u

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
      eta2 = eta * eta
      sint2 = eta2 * sini2
      
      cost = let c = sqrt $ max 0 (1 - sint2)
             in if entering then -c else c
      wi = mkV (eta * (-wox), eta * (-woy), cost)
      fr = frDielectric ei et $ if adj then cosTheta wo else cost
      f' = (white - fr) * t
      
      f = if adj
            then sScale f' $ abs (cosTheta wo / cost)
            else sScale f' eta2

