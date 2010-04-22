
module Camera(Camera, View(..), pinHoleCamera) where

import Math

type Camera = (Float, Float) -> Ray

-- | Defines the view for projective camera models
data View = View {
   viewPos :: Point, -- ^ the position of the camera in world space
   viewLookAt :: Normal, -- ^ the "look-at" point world space
   viewUp :: Normal, -- ^ the "up" vector, perpendicular to look-at
   viewDist :: Float, -- ^ the observer's distance from the image plane
   viewAspect :: Float
   }
   
-- | computes a point on the image plane
viewPoint :: View -> (Float, Float) -> Point
viewPoint view (u, v) = center `add` (scalMul right u') `add` (scalMul up' v') where
   center = (viewPos view) `add` (scalMul dir (viewDist view))
   right =  normalize $ (viewUp view) `cross` dir
   up' = cross right dir
   dir = normalize $ sub (viewLookAt view) (viewPos view)
   u' = u * (viewAspect view) - 0.5
   v' = v - 0.5

pinHoleCamera :: View -> Camera
pinHoleCamera view uv = Ray ro rd 0 infinity where
   ro = (viewPos view)
   rd = normalize $ sub pv ro
   pv = viewPoint view uv
