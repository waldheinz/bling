
module Graphics.Bling.Reflection.Microfacet (

   -- * Microfacet Distributions
   
   Distribution, mkBlinn, mkAnisotropic,    

   -- * Microfacet Distribution based BxDFs
   
   mkMicrofacet, mkFresnelBlend
   
   ) where

import Graphics.Bling.Fresnel
import Graphics.Bling.Montecarlo   
import Graphics.Bling.Random
import Graphics.Bling.Reflection

mkMicrofacet :: Distribution -> Fresnel -> Spectrum -> BxDF
mkMicrofacet dist fr r = mkBxDF [Reflection, Glossy] e s p where
   e wo wi
      | costi == 0 || costo == 0 = black
      | vx wh' == 0 && vy wh' == 0 && vz wh' == 0 = black
      | cosTheta wh < 0 = black
      | otherwise = sScale (r * fr costh) x
      where
         x = mfDistD dist wh * mfG wo wi wh / (4 * costi)
         costo = absCosTheta wo
         costi = absCosTheta wi
         wh' = wi + wo
         wh = normalize $ wh'
         costh = wi `dot` wh

   p wo wi 
      | sqLen wh' == 0 = 0
      | cosTheta wh < 0 = 0
      | otherwise = mfDistPdf dist wh / (4 * absDot wo wh)
      where
         wh' = wo + wi
         wh = normalize wh'
         
   s adj wo u
      | sameHemisphere wo wi = (f, wi, pdf / (4 * abs costH))
      | otherwise = (black, wo, 0)
      where
         (wh', d, pdf) = mfDistSample dist u
         wh = if cosTheta wh' < 0 then -wh' else wh'
         wi = (2 * wo `dot` wh) *# wh - wo
         costH = wo `dot` wh
         fact = d * (abs costH) / pdf * mfG wo wi wh
         f' = r * fr costH
         f = if adj
            then sScale f' (fact / absCosTheta wo)
            else sScale f' (fact / absCosTheta wi)

mkFresnelBlend
   :: Spectrum       -- ^ diffuse layer
   -> Spectrum       -- ^ specular reflection
   -> Spectrum       -- ^ absorption
   -> Distribution   -- ^ MF distribution
   -> Float          -- ^ thickness of coating
   -> BxDF
mkFresnelBlend rd rs ra dist depth = mkBxDF [Reflection, Glossy] e s p where
   
   e wo wi = diff + spec
      where
         costi = absCosTheta wi
         costo = absCosTheta wo

         -- absorption
         a = if depth > 0
            then sExp (sScale ra (-depth * (costi + costo) / (costi * costo)))
            else white
         
         -- diffuse
         diff = sScale (a * rd * (white - rs)) $ (28 / 23 * pi) *
               (1 - ((1 - 0.5 * costi) ** 5)) *
               (1 - ((1 - 0.5 * costo) ** 5))
            
         -- specular
         wh = let wh' = normalize $ wi + wo in if vz wh' < 0 then (-wh') else wh'
         costih = wi `absDot` wh
         spec = sScale schlick $
            mfDistD dist wh * costo / (4 * costih * max costi costo)
         schlick = rs + sScale (white - rs) ((1 - costih) ** 5)
      
   s adj wo (u1, u2)
      | pdf' == 0 = (black, wi, 0)
      | otherwise = (sScale f (1 / pdf), wi, pdf)
      where
         f = if adj then e wi wo else e wo wi
         pdf = 0.5 * (absCosTheta wi * invPi + pdf' / (4 * absDot wo wh))
         (pdf', wh, wi) = if u1 < 0.5
            then
               let wis = toSameHemisphere wo $ cosineSampleHemisphere (u1 * 2, u2) in
               let whs = let wh' = normalize $ wi + wo in if vz wh' < 0 then -wh' else wh' in
               (mfDistPdf dist whs, whs, wis)
            else
               let (whs, _, pdfs) = mfDistSample dist (2 * (u1 - 0.5), u2)
               in (pdfs, whs, 2 * wo `dot` wh *# wh - wo)
               
   p wo wi
      | sameHemisphere wo wi =
         0.5 * (absCosTheta wi * invPi + mfDistPdf dist wh / (4 * absDot wo wh))
      | otherwise = 0
      where
         wh = let wh' = normalize $ wi + wo in if vz wh' < 0 then -wh' else wh'
--------------------------------------------------------------------------------
-- Microfacet Distributions
--------------------------------------------------------------------------------

mfG :: Vector -> Vector -> Vector -> Float
mfG wo wi wh = min 1 $ min
      (2 * nDotWh * nDotWo / woDotWh)
      (2 * nDotWh * nDotWi / woDotWh) where
         nDotWh = absCosTheta wh
         nDotWo = absCosTheta wo
         nDotWi = absCosTheta wi
         woDotWh = wo `absDot` wh

data Distribution
   = Blinn        {-# UNPACK #-} !Float -- ^ e
   | Anisotropic  {-# UNPACK #-} !Float {-# UNPACK #-} !Float -- ^ ex and ey

-- | fixes the exponent for microfacet distribution to a usable range
fixExponent :: Float -> Float
fixExponent e = if e > 10000 || isNaN e then 10000 else e

-- | create a Blinn microfacet distribution
mkBlinn
   :: Float -- ^ the exponent in [0..10000]
   -> Distribution
mkBlinn e = Blinn $ fixExponent e

mkAnisotropic :: Float -> Float -> Distribution
mkAnisotropic ex ey = Anisotropic (fixExponent ex) (fixExponent ey)

mfDistPdf :: Distribution -> Vector -> Float
mfDistPdf (Anisotropic ex ey) wh = pdf where
   costh = absCosTheta wh
   (whx, why) = (vx wh, vy wh)
   e = (ex * whx * whx + ey * why * why) / (max 0 $ 1 - costh * costh)
   pdf = sqrt ((ex + 1) * (ey + 1)) * invTwoPi * (costh ** e)
   
mfDistPdf (Blinn e) wh = (e + 1) * (cost ** e) * invTwoPi where
   cost = absCosTheta wh
   
mfDistSample :: Distribution -> Rand2D -> (Vector, Float, Float)

mfDistSample (Anisotropic ex ey) (u1, u2) = (wh, d, pdf)
   where
      wh = sphericalDirection sint cost phi
      sint = sqrt $ max 0 (1 - cost * cost)
      ds = 1 - cost * cost
      f = invTwoPi * (cost ** e)
      d = sqrt ((ex + 2) * (ey + 2)) * f
      pdf = sqrt ((ex + 1) * (ey + 1)) * f 
      (whx, why) = (vx wh, vy wh)
      e = (ex * whx * whx + ey * why * why) / ds
      
      (phi, cost)
         | u1 < 0.25 = smpFirstQuadrand (4 * u1) id
         | u1 < 0.50 = smpFirstQuadrand (4 * (0.5 - u1)) (\x -> pi - x)
         | u1 < 0.75 = smpFirstQuadrand (4 * (u1 - 0.5)) (\x -> x + pi)
         | otherwise = smpFirstQuadrand (4 * (1 - u1)) (\x -> twoPi - x)
         
      smpFirstQuadrand u1' rsmp = (rsmp p, c) where
         p = if ex == ey
                then pi * u1' * 0.5
                else atan $ (sqrt $ (ex + 1) / (ey + 1)) * tan (pi * u1' * 0.5)
         (cp, sp) = (cos p, sin p)
         c = u2 ** (1 / (ex * cp * cp + ey * sp * sp + 1))
      
mfDistSample (Blinn e) (u1, u2) = (wh, d, pdf) where
   cost = u1 ** (1 / (e + 1))
   sint = sqrt $ max 0 (1 - cost * cost)
   phi = u2 * 2 * pi   
   wh = sphericalDirection sint cost phi
   f = (cost ** e) * invTwoPi
   d = (e + 2) * f
   pdf = (e + 1) * f
   
mfDistD :: Distribution -> Vector -> Float
mfDistD (Anisotropic ex ey) wh
   | d == 0 = 0
   | otherwise = sqrt ((ex + 2) * (ey + 2)) * invTwoPi * (costh ** e)
   where
      costh = absCosTheta wh
      d = 1 - costh * costh
      (whx, why) = (vx wh, vy wh)
      e = (ex * whx * whx + ey * why * why) / d
      
mfDistD (Blinn e) wh = (e + 2) * invTwoPi * (costh ** e) where
   costh = absCosTheta wh

