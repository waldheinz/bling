
module Graphics.Bling.SunSky (
   -- * Perez' physically based Sun / Sky model
   mkSunSkyLight
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture

-- | creates the Perez sun/sky model
mkSunSkyLight
   :: Vector -- ^ the east vector
   -> Vector -- ^ the sun direction in world coordinates
   -> Flt -- ^ the sky's turbidity
   -> SpectrumMap
mkSunSkyLight east sdw turb = mkTextureMap (640, 480) eval where
   eval cc =
      (skySpectrum ssd $ (sphToDir $ cartToSph cc))-- +
 --     (sunSpectrum sdw sunR (sphToDir $ cartToSph cc))
   up = mkV (0, 1, 0)
   basis = coordinateSystem' (normalize up) (normalize east)
   ssd = initSky basis (normalize sdw) turb
   sunR = sunSpectrum' ssd turb

type Perez = (Flt, Flt, Flt, Flt, Flt)

data SkyData = SD
   { sunDir :: !Vector
   , sunTheta :: Flt
   , perezx :: !Perez
   , perezy :: !Perez
   , perezY :: !Perez
   , zenithx :: !Flt
   , zenithy :: !Flt
   , zenithY :: !Flt
   }

sunThetaMax :: Flt
sunThetaMax = sqrt $ max 0 (1 - sint2) where
   sint2 = radius / meanDistance
   radius = 6.955e5 -- km
   meanDistance = 1.496e8 -- 149,60 million km
   
initSky :: LocalCoordinates -> Vector -> Flt -> SkyData
initSky basis sdw t = SD sd st px py pY zx zy zY where
   sd = normalize $ worldToLocal basis sdw
   st = acos $ clamp (sd .! dimZ) (-1) 1
   (st2, st3, t2) = (st * st, st * st * st, t * t)
   chi = (4 / 9 - t / 120) * (pi - 2 * st)

   pY = ( 0.17872 * t - 1.46303, -0.35540 * t + 0.42749, -0.02266 * t + 5.32505,
          0.12064 * t - 2.57705, -0.06696 * t + 0.37027)
   px = (-0.01925 * t - 0.25922, -0.06651 * t + 0.00081, -0.00041 * t + 0.21247,
         -0.06409 * t - 0.89887, -0.00325 * t + 0.04517)
   py = (-0.01669 * t - 0.26078, -0.09495 * t + 0.00921, -0.00792 * t + 0.21023,
         -0.04405 * t - 1.65369, -0.01092 * t + 0.05291)

   zY = ((4.04530 * t - 4.97100) * tan chi  - 0.2155 * t + 2.4192) * 1000
   zx = ( 0.00165 * st3 - 0.00374 * st2 + 0.00208 * st          ) * t2 +
        (-0.02902 * st3 + 0.06377 * st2 - 0.03202 * st + 0.00394) * t  +
        ( 0.11693 * st3 - 0.21196 * st2 + 0.06052 * st + 0.25885)
   zy = ( 0.00275 * st3 - 0.00610 * st2 + 0.00316 * st          ) * t2 +
        (-0.04212 * st3 + 0.08970 * st2 - 0.04153 * st + 0.00515) * t  +
        ( 0.15346 * st3 - 0.26756 * st2 + 0.06669 * st + 0.26688)

skySpectrum :: SkyData -> Vector -> Spectrum
skySpectrum ssd dir@(Vector _ _ dz')
   | dz < 0.001 = black
   | otherwise = fromXYZ (x', y', z')
   where
      dz = -dz'
      (cx, cy, cz) = chromaticityToXYZ x y
      (x', z') = (cx * y' / cy, cz * y' / cy)
      theta = acos dz
      gamma = acos $ clamp (dir `dot` sunDir ssd) (-1) 1
      x = perez (perezx ssd) (sunTheta ssd) theta gamma (zenithx ssd)
      y = perez (perezy ssd) (sunTheta ssd) theta gamma (zenithy ssd)
      y' = perez (perezY ssd) (sunTheta ssd) theta gamma (zenithY ssd) * 1e-4

perez :: Perez -> Flt -> Flt -> Flt -> Flt -> Flt
perez (p0, p1, p2, p3, p4) sunT t g lvz = lvz * num / den where
   num = (1 + p0 * exp (p1 / cos t)) * (1 + p2 * exp (p3 * g)) + p4 * csg * csg
   den = (1 + p0 * exp p1) * (1 + p2 * exp (p3 * sunT)) + p4 * cst * cst
   csg = cos g
   cst = cos sunT

sunSpectrum :: Vector -> Spectrum -> Vector -> Spectrum
sunSpectrum sunD r dir
   | (sunD `dot` dir) > sunThetaMax = r
   | otherwise = black
   
sunSpectrum' :: SkyData -> Flt -> Spectrum
sunSpectrum' ssd turb
   | (sunDir ssd) .! dimZ < 0 = black -- below horizon
   | otherwise = fromSpd $ mkSpdFunc sf
   where
      t = sunTheta ssd
      sf l = (evalSpd solCurve l) * tR * tA * tO * tG * tWA where
         -- relative optical mass
         m = 1 / (cos t + 0.000940 * ((1.6386 - t) **  (-1.253)))

         -- rayleigh scattering
         tR = exp(-m * 0.008735 * ((l / 1000) ** (-4.08)))

         -- aerosol (water + dust) attenuation
         alpha = 1.3
         beta = 0.04608365822050 * turb - 0.04586025928522
         tA = exp(-m * beta * ((l / 1000) ** (-alpha)))

         -- attenuation due to ozone absorption
         lozone = 0.35
         tO = exp $ -m * (evalSpd koCurve l) * lozone

         -- attenuation due to mixed gases absorption
         kg = evalSpd kgCurve l
         tG = exp $ -1.41 * kg * m / ((1.0 + 118.93 * kg * m) ** 0.45)

         -- attenuation due to water vapor absorption
         kwa = evalSpd kwaCurve l
         w = 2
         tWA = exp $ -0.2385 * kwa * w * m / ((1 + 20.07 * kwa * w * m) ** 0.45)

solCurve :: Spd
solCurve = mkSpd' amps 380 750 where
   amps = [ 165.5, 162.3, 211.2, 258.8, 258.2, 242.3, 267.6, 296.6, 305.4,
            300.6, 306.6, 288.3, 287.1, 278.2, 271.0, 272.3, 263.6, 255.0,
            250.6, 253.1, 253.5, 251.3, 246.3, 241.7, 236.8, 232.1, 228.2,
            223.4, 219.7, 215.3, 211.0, 207.3, 202.4, 198.7, 194.3, 190.7,
            186.3, 182.6 ]

koCurve :: Spd
koCurve = mkSpd $ zip ls as where
   ls = [ 300, 305, 310, 315, 320, 325, 330, 335, 340, 345, 350, 355, 445, 450,
          455, 460, 465, 470, 475, 480, 485, 490, 495, 500, 505, 510, 515, 520,
          525, 530, 535, 540, 545, 550, 555, 560, 565, 570, 575, 580, 585, 590,
          595, 600, 605, 610, 620, 630, 640, 650, 660, 670, 680, 690, 700, 710,
          720, 730, 740, 750, 760, 770, 780, 790 ]
   as = [ 10.0, 4.8, 2.7, 1.35, 0.8, 0.380, 0.160, 0.075, 0.04, 0.019, 0.007, 0,
          0.003, 0.003, 0.004, 0.006, 0.008, 0.009, 0.012, 0.014, 0.017, 0.021,
          0.025, 0.03, 0.035, 0.04, 0.045, 0.048, 0.057, 0.063, 0.07, 0.075,
          0.08, 0.085, 0.095, 0.103, 0.110, 0.12, 0.122, 0.12, 0.118, 0.115,
          0.12, 0.125, 0.130, 0.12, 0.105, 0.09, 0.079, 0.067, 0.057, 0.048,
          0.036, 0.028, 0.023, 0.018, 0.014, 0.011, 0.010, 0.009, 0.007,
          0.004, 0, 0 ]

kgCurve :: Spd
kgCurve = mkSpd $ zip [759, 760, 770, 771] [0, 3.0, 0.210, 0]

kwaCurve :: Spd
kwaCurve = mkSpd $ zip ls as where
   ls = [ 689, 690, 700, 710, 720, 730, 740, 750, 760, 770, 780, 790, 800 ]
   as = [ 0, 0.160e-1, 0.240e-1, 0.125e-1, 0.100e+1, 0.870, 0.610e-1,
          0.100e-2, 0.100e-4, 0.100e-4, 0.600e-3, 0.175e-1, 0.360e-1 ]

