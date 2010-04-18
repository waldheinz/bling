module Bsdf(Bsdf(..), BsdfSample(..), sampleBsdf, evalBsdf, localToWorld, worldToLocal) where

import Math
import Transport
import Random

data BsdfSample = BsdfSample {
   sampledType :: BxdfType,
   pdf :: Float,
   transport :: Spectrum,
   wi :: Vector
   }

data Bsdf = Bsdf AnyBxdf LocalCoordinates

sampleBsdf :: Bsdf -> Vector -> Rand BsdfSample
sampleBsdf (Bsdf bxdf sc) woW = do
   (BxdfSample wi pdf) <- bxdfSample bxdf (worldToLocal sc wo)
   return (BsdfSample bt pdf (bxdfEval bxdf wo wi) (localToWorld sc wi))
      where
         bt = bxdfType bxdf
         wo = worldToLocal sc woW
         
evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf (Bsdf bxdf sc) woW wiW = bxdfEval bxdf wo wi where
   wo = worldToLocal sc woW
   wi = worldToLocal sc wiW
         
worldToLocal :: LocalCoordinates -> Vector -> Vector
worldToLocal (LocalCoordinates nn' sn' tn') v = (dot v sn', dot v tn', dot v nn')

localToWorld :: LocalCoordinates -> Vector -> Vector
localToWorld (LocalCoordinates (nx, ny, nz) (sx, sy, sz) (tx, ty, tz)) (x, y, z) =
   (sx * x + tx * y + nx * z,
    sy * x + ty * y + ny * z,
    sz * x + tz * y + nz * z)
