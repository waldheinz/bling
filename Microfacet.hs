
module Microfacet where

import Color
import Math
import Random
import Specular
import Transport

data Microfacet = Microfacet {
   microfacetDistribution :: MfDistribution,
   microfacetFresnel :: Fresnel,
   microfacetReflectance :: Spectrum
   }

instance Bxdf Microfacet where
   bxdfType _ = mkBxdfType [Reflection, Glossy]
   bxdfEval (Microfacet d fresnel r) wo wi = sScale (r * f) ((mfDistD d wh) * (mfG wo wi wh) / (4 * costi * costo)) where
      costo = abs $ cosTheta wo
      costi = abs $ cosTheta wi
      wh = normalize $ add wo wi
      f = fresnel costh
      costh = dot wi wh
      
   bxdfSample mf wo dirU = bxdfSample' dirU mf wo
   
   bxdfPdf (Microfacet d _ _) wo wi
      | sameHemisphere wo wi = mfDistPdf d wo wi
      | otherwise = infinity
   
bxdfSample' :: Rand2D -> Microfacet -> Vector -> (Spectrum, Vector, Float)
bxdfSample' dirU mf@(Microfacet d _ _) wo
   | sameHemisphere wo wi = (f, wi, pdf)
   | otherwise = (black, undefined, infinity)
   where
         f = bxdfEval mf wo wi
         (pdf, wi) = mfDistSample d dirU wo
      
mfG :: Vector -> Vector -> Vector -> Float
mfG wo wi wh = min 1 $ min (2 * nDotWh * nDotWo / woDotWh) (2 * nDotWh * nDotWi / woDotWh) where
   nDotWh = abs $ cosTheta wh
   nDotWo = abs $ cosTheta wo
   nDotWi = abs $ cosTheta wi
   woDotWh = absDot wo wh

data MfDistribution = Blinn Float

mfDistPdf :: MfDistribution -> Vector -> Vector -> Float
mfDistPdf (Blinn e) wo wi = (e + 2) * (cost ** e) / (2 * pi * 4 * (dot wo h)) where
   h@(_, _, hz) = normalize $ add wo wi
   cost = abs hz

mfDistSample :: MfDistribution -> Rand2D -> Vector -> (Float, Vector)
mfDistSample (Blinn e) (u1, u2) wo = (pdf, wi) where
   pdf = (e + 2) * (cost ** e) / (2 * pi * 4 * (dot wo h)) -- possible divide by zero?
   wi = add (neg wo) (scalMul h (2 * dot h wo))
   h = toSameHemisphere wo $ sphericalDirection sint cost phi
   cost = u1 ** (1 / (e + 1))
   sint = sqrt $ max 0 (1 - cost * cost)
   phi = u2 * 2 * pi
   
mfDistD :: MfDistribution -> Vector -> Float
mfDistD (Blinn e) wh = (e + 2) * invTwoPi * (costh ** e) where
   costh = abs $ cosTheta wh
