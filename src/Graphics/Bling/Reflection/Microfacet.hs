
module Graphics.Bling.Reflection.Mircofacet (

   -- * Microfacet Distribution based BRDF
   
   mkMicrofacet, Distribution, mkBlinn, mkAnisotropic,    mkFresnelBlend,
   
   ) where
   

data Micro = Microfacet !Distribution !Fresnel !Spectrum
   | FresnelBlend !Spectrum !Spectrum !Distribution

mkMicrofacet :: Distribution -> Fresnel -> Spectrum -> Bxdf
mkMicrofacet = Microfacet

mkFresnelBlend :: Spectrum -> Spectrum -> Distribution -> Bxdf
mkFresnelBlend = FresnelBlend


bxdfEval (Microfacet d fresn r) wo wi
   | costi == 0 || costo == 0 = black
   | vx wh' == 0 && vy wh' == 0 && vz wh' == 0 = black
   | otherwise = sScale (r * fresn costh) x
   where
      x = mfDistD d wh * mfG wo wi wh / (4 * costi * costo)
      costo = absCosTheta wo
      costi = absCosTheta wi
      wh' = wi + wo
      wh = normalize $ wh'
      costh = wi `dot` wh

bxdfEval (FresnelBlend rd rs d) wo wi
   | vx wh' == 0 && vy wh' == 0 && vz wh' == 0 = black
   | otherwise = diff + spec
   where
      costi = absCosTheta wi
      costo = absCosTheta wo
      wh' = wi + wo
      wh = normalize $ wh'
      diff = sScale (rd * (white - rs)) $ (28 / 23 * pi) *
               (1 - ((1 - 0.5 * costi) ** 5)) *
               (1 - ((1 - 0.5 * costo) ** 5))

      spec = sScale schlick $ mfDistD d wh / (4 * wi `absDot` wh) * (max costi costo)
      cost = wi `dot` wh
      schlick = rs + sScale (white - rs) ((1 - cost) ** 5)

bxdfSample mf@(Microfacet d _ _) wo dirU
   | sameHemisphere wo wi = (f, wi, pdf)
   | otherwise = (black, wo, 0)
   where
         f = bxdfEval mf wo wi
         (pdf, wi) = mfDistSample d dirU wo


bxdfSample fb@(FresnelBlend _ _ d) wo (u1, u2) = (f, wi, pdf) where
   pdf = bxdfPdf fb wo wi
   f = bxdfEval fb wo wi
   wi = if u1 < 0.5
           then toSameHemisphere wo $ cosineSampleHemisphere (u1 * 2, u2)
           else snd $ mfDistSample d (2 * (u1 - 0.5), u2) wo


bxdfSample' mf@(Microfacet _ _ _) wo u = bxdfSample mf wo u

bxdfSample' fb@(FresnelBlend _ _ _) wo u = bxdfSample fb wo u

bxdfPdf (FresnelBlend _ _ d) wo wi
   | sameHemisphere wo wi = 0.5 * (absCosTheta wi * invPi + mfDistPdf d wo wi)
   | otherwise = 0

bxdfPdf (Microfacet d _ _) wo wi 
   | sameHemisphere wo wi = mfDistPdf d wo wi
   | otherwise = 0


bxdfType (Microfacet _ _ _)   = mkBxdfType [Reflection, Glossy]
bxdfType (FresnelBlend _ _ _) = mkBxdfType [Reflection, Glossy]

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

mfDistPdf :: Distribution -> Vector -> Vector -> Float
mfDistPdf (Anisotropic ex ey) wo wi
   | ds > 0 && wo `dot` wh > 0 = d / (4 * (wo `dot` wh))
   | otherwise = 0
   where
      wh = normalize $ wo + wi
      costh = absCosTheta wh
      ds = 1 - costh * costh
      (whx, why) = (vx wh, vy wh)
      e = (ex * whx * whx + ey * why * why) / ds
      d = sqrt ((ex + 1) * (ey + 1)) * invTwoPi * (costh ** e)
      
mfDistPdf (Blinn e) wo wi = (e + 2) * (cost ** e) / (2 * pi * 4 * dot wo h) where
   h@(Vector _ _ hz) = normalize $ wo + wi
   cost = abs hz
   
mfDistSample :: Distribution -> Rand2D -> Vector -> (Float, Vector)

mfDistSample (Anisotropic ex ey) (u1, u2) wo
   | ds > 0 && wo `dot` wh > 0 = (d / (4 * wo `dot` wh), wi)
   | otherwise = (0, wi)
   where
      wi = -wo + (wh * (vpromote $ 2 * (wo `dot` wh)))
      wh' = sphericalDirection sint cost phi
      wh = if sameHemisphere wo wh' then wh' else -wh'
      sint = sqrt $ max 0 (1 - cost * cost)
      ds = 1 - cost * cost
      d = sqrt ((ex + 1) * (ey + 1)) * invTwoPi * (cost ** e)
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
      
mfDistSample (Blinn e) (u1, u2) wo = (pdf, wi) where
   pdf = (e + 2) * (cost ** e) / (2 * pi * 4 * dot wo h) -- possible divide by zero?
   wi = (-wo) + (h * vpromote (2 * dot h wo))
   h = toSameHemisphere wo $ sphericalDirection sint cost phi
   cost = u1 ** (1 / (e + 1))
   sint = sqrt $ max 0 (1 - cost * cost)
   phi = u2 * 2 * pi

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

