module Bsdf(Bsdf(..), BsdfSample(..), sampleBsdf, evalBsdf) where

-- import Material
import Math
import Transport
import Random

data BsdfSample = BsdfSample {
   sampledType :: BxdfType,
   pdf :: Float,
   transport :: Spectrum,
   wi :: Vector
   }

data Bsdf = Bsdf AnyBxdf ShadingCoordinates

sampleBsdf :: Bsdf -> Vector -> Rand BsdfSample
sampleBsdf (Bsdf bxdf sc) wo = do
   (BxdfSample wi pdf) <- bxdfSample bxdf (worldToLocal sc wo)
   return (BsdfSample bt pdf (bxdfEval bxdf wo wi) (localToWorld sc wi))
      where
         bt = bxdfType bxdf
         
evalBsdf :: Bsdf -> Vector -> Vector -> Spectrum
evalBsdf (Bsdf bxdf sc) woW wiW = bxdfEval bxdf wo wi where
   wo = worldToLocal sc woW
   wi = worldToLocal sc wiW
         
worldToLocal :: ShadingCoordinates -> Vector -> Vector
worldToLocal (ShadingCoordinates nn' sn' tn') v = (dot v sn', dot v tn', dot v nn')

localToWorld :: ShadingCoordinates -> Vector -> Vector
localToWorld (ShadingCoordinates (nx, ny, nz) (sx, sy, sz) (tx, ty, tz)) (x, y, z) =
   (sx * x + tx * y + nx * z, sy * x + ty * y + ny * z, sz * x + tz * y + nz * z)
