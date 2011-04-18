
module Graphics.Bling.Camera(
   Camera, mkPerspectiveCamera, ppCamera,

   fireRay
   ) where

import Text.PrettyPrint

import Graphics.Bling.Math
import Graphics.Bling.Random
import Graphics.Bling.Transform

data Camera
      = ProjectiveCamera {
         _cam2world :: Transform,
         _cam2screen :: Transform,
         _raster2cam :: Transform,
         _screen2raster :: Transform,
         _raster2screen :: Transform,
         _lensRadius :: Flt,
         _focalDistance :: Flt }

ppCamera :: Camera -> Doc
ppCamera (ProjectiveCamera c2w _ _ _ _ lr fd) = vcat [
   text "Projective",
   text "Cam2World" <+> text (show c2w),
   text "Lens Radius" <+> float lr,
   text "Focal Distance" <+>  float fd ]
   
fireRay :: Camera -> CameraSample -> Ray
fireRay
   (ProjectiveCamera c2w _ r2c _ _ lr fd)
   (CameraSample ix iy luv) = transRay c2w r where
      r = if lr > 0 then ray' else ray
      
      -- ray without accounting for lens size
      ray = Ray (mkPoint 0 0 0) (normalize pCamera) 0 infinity
      pCamera = transPoint r2c pRaster
      pRaster = mkPoint ix iy 0
      
      -- account for lens size
      ray' = Ray ro rd 0 infinity
      ro = mkPoint lu lv 0
      (lu, lv) = (\(u', v') -> (u'*lr, v'*lr)) (concentricSampleDisk luv)
      rd = normalize $ pFocus - ro
      pFocus = rayAt ray (fd / (vz $ rayDir ray))

mkProjective
   :: Transform -- ^ camera to world
   -> Transform -- ^ the projection
   -> Flt -- ^ lens radius
   -> Flt -- ^ focal distance
   -> Flt -- ^ frame size in x
   -> Flt -- ^ frame size in y
   -> Camera
   
mkProjective c2w p lr fd sx sy = ProjectiveCamera c2w p r2c s2r r2s lr fd where
   s2r = t `concatTrans` st2 `concatTrans` st1
   aspect = sx / sy
   (s0, s1, s2, s3) = if aspect > 1
                         then (-aspect, aspect, -1, 1)
                         else (-1, 1, -1 / aspect, 1 / aspect)
   st1 = scale (Vector sx sy 1)
   st2 = scale (Vector (1 / (s1 - s0)) (1 / (s2 - s3)) 1)
   t = translate (Vector (-s0) (-s3) 0)
   r2s = inverse s2r
   r2c = r2s `concatTrans` (inverse p)
   
mkPerspectiveCamera
   :: Transform -- ^ the camera to world transform
   -> Flt -- ^ lens radius
   -> Flt -- ^ focal distance
   -> Flt -- ^ field of view in degrees
   -> Flt -- ^ frame size in x
   -> Flt -- ^ frame size in y
   -> Camera

mkPerspectiveCamera c2w lr fd fov sx sy = mkProjective c2w p lr fd sx sy where
   p = perspective fov 1e-2 1000
