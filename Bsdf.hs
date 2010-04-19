module Bsdf(Bsdf(..), BsdfSample(..), sampleBsdf, evalBsdf, localToWorld, worldToLocal) where

import Math
import Transport
import Random

data BsdfSample = BsdfSample {
   bsdfsampleType :: BxdfType,
   bsdfSamplePdf :: Float,
   bsdfSampleTransport :: Spectrum,
   bsdfSampleWi :: Vector
   }

data Bsdf = Bsdf AnyBxdf LocalCoordinates

sampleBsdf :: Bsdf -> Vector -> Rand BsdfSample
sampleBsdf (Bsdf bxdf sc) woW = do
   (BxdfSample wi pdf) <- bxdfSample bxdf wo
   return (BsdfSample bt pdf (bxdfEval bxdf wo wi) (localToWorld sc wo))
      where
         bt = bxdfType bxdf
         wo = worldToLocal sc woW
         
evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf (Bsdf bxdf sc) woW wiW
   | isReflection bxdf = bxdfEval bxdf wo wi 
   | otherwise = black
   where
      wo = worldToLocal sc woW
      wi = worldToLocal sc wiW
      
worldToLocal :: LocalCoordinates -> Vector -> Vector
worldToLocal (LocalCoordinates sn tn nn) v = (dot v sn, dot v tn, dot v nn)

localToWorld :: LocalCoordinates -> Vector -> Vector
localToWorld (LocalCoordinates (sx, sy, sz) (tx, ty, tz) (nx, ny, nz)) (x, y, z) =
   (sx * x + tx * y + nx * z,
    sy * x + ty * y + ny * z,
    sz * x + tz * y + nz * z)
