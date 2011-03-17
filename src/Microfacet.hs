
module Microfacet(Microfacet(..), Distribution(..)) where

import Math
import Random
import Specular
import Spectrum
import Transport

data Microfacet = Microfacet {
   distribution :: Distribution,
   fresnel :: Fresnel,
   reflectance :: Spectrum
   }

instance Bxdf Microfacet where
   bxdfType _ = mkBxdfType [Reflection, Glossy]
   bxdfEval (Microfacet d fr r) wo wi = sScale (r * fr costh) x where
      x = mfDistD d wh * mfG wo wi wh / (4 * costi * costo)
      costo = abs $ cosTheta wo
      costi = abs $ cosTheta wi
      wh = normalize $ add wi wo
      costh = dot wi wh
      
   bxdfSample mf wo dirU = bxdfSample' dirU mf wo
   
   bxdfPdf (Microfacet d _ _) wo wi
      | sameHemisphere wo wi = mfDistPdf d wo wi
      | otherwise = 0
   
bxdfSample' :: Rand2D -> Microfacet -> Vector -> (Spectrum, Vector, Float)
bxdfSample' dirU mf@(Microfacet d _ _) wo
   | sameHemisphere wo wi = (f, wi, pdf)
   | otherwise = (black, wo, 0)
   where
         f = bxdfEval mf wo wi
         (pdf, wi) = mfDistSample d dirU wo
      
mfG :: Vector -> Vector -> Vector -> Float
mfG wo wi wh = min 1 $ min (2 * nDotWh * nDotWo / woDotWh) (2 * nDotWh * nDotWi / woDotWh) where
   nDotWh = abs $ cosTheta wh
   nDotWo = abs $ cosTheta wo
   nDotWi = abs $ cosTheta wi
   woDotWh = absDot wo wh

data Distribution = Blinn Float

mfDistPdf :: Distribution -> Vector -> Vector -> Float
mfDistPdf (Blinn e) wo wi = (e + 2) * (cost ** e) / (2 * pi * 4 * dot wo h) where
   h@(MkVector _ _ hz) = normalize $ add wo wi
   cost = abs hz

mfDistSample :: Distribution -> Rand2D -> Vector -> (Float, Vector)
mfDistSample (Blinn e) (u1, u2) wo = (pdf, wi) where
   pdf = (e + 2) * (cost ** e) / (2 * pi * 4 * dot wo h) -- possible divide by zero?
   wi = add (neg wo) (scalMul h (2 * dot h wo))
   h = toSameHemisphere wo $ sphericalDirection sint cost phi
   cost = u1 ** (1 / (e + 1))
   sint = sqrt $ max 0 (1 - cost * cost)
   phi = u2 * 2 * pi
   
mfDistD :: Distribution -> Vector -> Float
mfDistD (Blinn e) wh = (e + 2) * invTwoPi * (costh ** e) where
   costh = abs $ cosTheta wh
