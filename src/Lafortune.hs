
module Lafortune (
   measuredMaterial, Measured(..)
   ) where

import Geometry
import Material
import Math
import Spectrum
import Transport

import Data.List (foldl')

data Lobe = Lobe
   Spectrum -- ^ X
   Spectrum -- ^ Y
   Spectrum -- ^ Z
   Spectrum -- ^ exponent
   
data Lafortune = Lafortune Spectrum [Lobe]

instance Bxdf Lafortune where
   
   bxdfType _ = mkBxdfType [Reflection, Glossy]
   
   bxdfEval (Lafortune diffuse lobes) (MkVector wox woy woz) (MkVector wix wiy wiz) =
      foldl' (+) (sScale diffuse invPi) $ map evalLobe lobes where
         evalLobe (Lobe lX lY lZ lE) = sPow v lE where
            v = vx + vy + vz
            vx = sScale lX (wox * wix)
            vy = sScale lY (woy * wiy)
            vz = sScale lZ (woz * wiz)
            
data Measured = BrushedMetal | BluePaint | Felt | Clay | Primer

measuredMaterial :: Measured -> Material
measuredMaterial Primer int = mkBsdf [bxdf] sc where
      sc = shadingCs int
      bxdf = MkAnyBxdf $ Lafortune (fromRGB (0.118230, 0.121218, 0.133209)) lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = fromRGB ( -0.399286, -1.033473, -1.058104 )
      z1  = fromRGB (  0.167504,  0.009545, -0.068002 )
      e1  = fromRGB (  2.466633,  7.637253,  8.117645 )
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = fromRGB ( -1.041861, -1.100108, -1.087779 )
      z2  = fromRGB (  0.014375, -0.198147, -0.053605 )
      e2  = fromRGB (  7.993722, 29.446268, 41.988990 )
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = fromRGB ( -1.098605, -0.379883, -0.449038 )
      z3  = fromRGB ( -0.145110,  0.159127,  0.173224 )
      e3  = fromRGB ( 31.899719,  2.372852,  2.636161 )
   
measuredMaterial Clay int = mkBsdf [bxdf] sc where
      sc = shadingCs int
      bxdf = MkAnyBxdf $ Lafortune (fromRGB (0.383626, 0.260749, 0.274207)) lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = fromRGB (  -1.089701,  -1.102701,  -1.107603 )
      z1  = fromRGB (  -1.354682,  -2.714801,  -1.569866 )
      e1  = fromRGB (  17.968505,  11.024489,  21.270282 )
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = fromRGB (  -0.733381,  -0.793320,  -0.848206 )
      z2  = fromRGB (   0.676108,   0.679314,   0.726031 )
      e2  = fromRGB (   8.219745,   9.055139,  11.261951 )
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = fromRGB (  -1.010548,  -1.012378,  -1.011263 )
      z3  = fromRGB (   0.910783,   0.885239,   0.892451 )
      e3  = fromRGB ( 152.912795, 141.937171, 201.046802 )
   
measuredMaterial Felt int = mkBsdf [bxdf] sc where
      sc = shadingCs int
      bxdf = MkAnyBxdf $ Lafortune (fromRGB (0.025865, 0.025865, 0.025865)) lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = fromRGB (-0.304075, -0.304075, -0.304075)
      z1  = fromRGB (-0.065992, -0.065992, -0.065992)
      e1  = fromRGB (3.047892,  3.047892,  3.047892)
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = fromRGB (-0.749561, -0.749561, -0.749561)
      z2  = fromRGB (-1.167929, -1.167929, -1.167929)
      e2  = fromRGB (6.931827, 6.931827, 6.931827)
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = fromRGB (1.004921,  1.004921,  1.004921)
      z3  = fromRGB (-0.205529, -0.205529, -0.205529)
      e3  = fromRGB (94.117332, 94.117332, 94.117332)
   
measuredMaterial BluePaint int = mkBsdf [bxdf] sc where
      sc = shadingCs int
      bxdf = MkAnyBxdf $ Lafortune (fromRGB (0.3094, 0.39667, 0.70837)) lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = fromRGB (0.870567,   0.857255, 0.670982)
      z1  = fromRGB (0.803624,   0.774290, 0.586674)
      e1  = fromRGB (21.820103, 18.597755, 7.472717)
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = fromRGB (-0.451218, -0.406681, -0.477976)
      z2  = fromRGB (0.023123, 0.017625, 0.227295)
      e2  = fromRGB (2.774499, 2.581499, 3.677653)
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = fromRGB (-1.031545, -1.029426, -1.026588)
      z3  = fromRGB (0.706734, 0.696530, 0.687715)
      e3  = fromRGB (66.899060, 63.767912, 57.489181)
   
measuredMaterial BrushedMetal int = mkBsdf [bxdf] sc where
      sc = shadingCs int
      bxdf = MkAnyBxdf $ Lafortune black lobes
      
      lobes = [lobe1, lobe2, lobe3]
      
      lobe1 = Lobe xy1 xy1 z1 e1
      xy1 = fromRGB (-1.11854, -1.11845, -1.11999)
      z1  = fromRGB (1.01272,  1.01469,  1.01942)
      e1  = fromRGB (15.8708,  15.6489,  15.4571)
      
      lobe2 = Lobe xy2 xy2 z2 e2
      xy2 = fromRGB (-1.05334, -1.06409, -1.08378  )
      z2  = fromRGB (0.69541,   0.662178, 0.626672 )
      e2  = fromRGB (111.267,  88.9222,  65.2179   )
      
      lobe3 = Lobe xy3 xy3 z3 e3
      xy3 = fromRGB (  -1.01684 ,  -1.01635 ,  -1.01529 )
      z3  = fromRGB (   1.00132 ,   1.00112 ,   1.00108 )
      e3  = fromRGB ( 180.181   , 184.152   , 195.773   )
      
 
