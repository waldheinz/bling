
module Camera(Camera, View(..), pinHoleCamera) where

import Math

-- | a camera transforms a pixel in normalized device coordinates to a ray
type Camera = (Float, Float) -> Ray

-- | defines the view for projective camera models
data View = View {
   viewPos :: Point, -- ^ the position of the camera in world space
   viewLookAt :: Normal, -- ^ the "look-at" point world space
   viewUp :: Normal, -- ^ the "up" vector
   viewFocalLength :: Float, -- ^ focal length
   viewAspect :: Float -- ^ aspect ratio of image plane
   } deriving (Show)
   
-- | computes a point on the image plane
viewPoint :: View -> (Float, Float) -> Point
viewPoint (View pos la up dist aspect) (u, v) = vp where
   vp = center `add` (scalMul right u') `add` (scalMul up' v')
   center = pos `add` (scalMul dir dist)
   right =  normalize $ up `cross` dir
   up' = cross right dir
   dir = normalize $ sub la pos
   u' = (u - 0.5) * aspect
   v' = v - 0.5
   
-- | a simple "pinhole" camera
pinHoleCamera :: View -> Camera
pinHoleCamera view uv = Ray pv rd 0 infinity where
   rd = normalize $ sub pv $ viewPos view
   pv = viewPoint view uv
   
