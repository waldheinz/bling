
module Graphics.Bling.Camera (

   -- * Creating Cameras
   
   Camera, mkPerspectiveCamera, mkEnvironmentCamera,
   
   -- * Using Cameras
   
   fireRay, CameraSampleResult(..), sampleCam
   ) where

import Control.Monad (liftM)
import Text.PrettyPrint

import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

-- | a @Camera@
data Camera
      = ProjectiveCamera {
         _cam2world :: Transform,
         _raster2cam :: Transform,
         _world2raster :: Transform,
         _pixelArea :: Flt, -- the area of a pixel
         _lensRadius :: Flt,
         _focalDistance :: Flt }
      | Environment Transform Flt Flt -- cam2world xres yres
      deriving (Show)
      
instance Printable Camera where
   prettyPrint (ProjectiveCamera _ _ _ ap lr fd) = vcat [
      text "Projective",
      text "Lens Radius" <+> float lr,
      text "Focal Distance" <+>  float fd,
      text "area of single pixel" <+> float ap ]

   prettyPrint (Environment _ _ _) = text "Environment"

--
-- sending eye rays into the scene
--

-- | fires an eye ray from a camera
fireRay :: Camera -> Sampled s Ray
fireRay (ProjectiveCamera c2w r2c _ _ lr fd) = {-# SCC "fireRayProjective" #-} do
   cs <- cameraSample
   let ix = imageX cs
   let iy = imageY cs
   let luv = lensUV cs
   return $ transRay c2w (r ix iy luv) where
      r ix iy luv = if lr > 0 then ray' else ray where
         
         -- ray without accounting for lens size
         ray = Ray (mkPoint (0, 0, 0)) (normalize pCamera) 0 infinity
         pCamera = transPoint r2c pRaster
         pRaster = mkPoint' ix iy 0
         
         -- account for lens size
         ray' = Ray ro rd 0 infinity
         ro = mkPoint' lu lv 0
         (lu, lv) = (\(u', v') -> (u'*lr, v'*lr)) (concentricSampleDisk luv)
         rd = normalize $ pFocus - ro
         pFocus = rayAt ray (fd / vz (rayDir ray))

fireRay (Environment c2w sx sy) = do
   ix <- imageX `liftM` cameraSample
   iy <- imageY `liftM` cameraSample
   return $ transRay c2w $ Ray (mkPoint' 0 0 0) (dir ix iy) 0 infinity where
      dir ix iy = mkV (sin t * cos p, cos t, sin t * sin p) where
         t = pi * iy / sy
         p = 2 * pi * ix / sx
         
--
-- sampling the camera
--

data CameraSampleResult = CameraSampleResult
   { csF             :: Spectrum -- ^ transport
   , csP             :: Point -- ^ point on lens
   , csImgX          :: Flt -- ^ pixel pos x
   , csImgY          :: Flt -- ^ pixel pos y
   , csPdf           :: Float
   }

sampleCam
   :: Camera -- ^ the camera to sample
   -> Point -- ^ the point in world space
   -> CameraSampleResult

sampleCam (ProjectiveCamera c2w r2c w2r ap _ _) p = smp where
   smp = CameraSampleResult white pLens px py (cost3 * ap)
   pRas@(Vector px py _) = transPoint w2r p
   pLens = transPoint c2w $ mkPoint' 0 0 0
   pRas' = transPoint r2c pRas
   (Vector _ _ cost) = normalize $ transVector (inverse c2w) pRas'
   cost3 = cost * cost * cost
   
sampleCam c _ = error $ "can not sample " ++ show c

--
-- creating cameras
--

mkProjective
   :: Transform -- ^ camera to world
   -> Transform -- ^ the projection
   -> Flt -- ^ lens radius
   -> Flt -- ^ focal distance
   -> Flt -- ^ frame size in x
   -> Flt -- ^ frame size in y
   -> Flt -- ^ focal length
   -> Camera
   
mkProjective c2w p lr fd sx sy fl = ProjectiveCamera c2w r2c w2r ap lr fd where
   s2r = t `concatTrans` st2 `concatTrans` st1
   aspect = sx / sy
   (s0, s1, s2, s3) = if aspect > 1
                         then (-aspect, aspect, -1, 1)
                         else (-1, 1, -1 / aspect, 1 / aspect)
   st1 = scale (Vector sx sy 1)
   st2 = scale (Vector (1 / (s1 - s0)) (1 / (s2 - s3)) 1)
   t = translate (Vector (-s0) (-s3) 0)
   r2s = inverse s2r
   r2c = r2s `concatTrans` inverse p
   w2r = concatTrans w2s s2r -- world to raster
   w2s = concatTrans (inverse c2w) p  -- world to screen
   ap = (pw * ph) -- pixel area
   pw = fl * (s1 - s0) / 2 / sx
   ph = fl * (s3 - s2) / 2 / sy
   
-- | creates a perspective camera using the specified parameters
mkPerspectiveCamera
   :: Transform -- ^ the camera to world transform
   -> Flt -- ^ lens radius
   -> Flt -- ^ focal distance
   -> Flt -- ^ field of view in degrees
   -> Flt -- ^ frame size in x
   -> Flt -- ^ frame size in y
   -> Camera
mkPerspectiveCamera c2w lr fd fov sx sy = mkProjective c2w p lr fd sx sy fl
   where
      p = perspective fov 1e-2 1000
      fl = tan ((radians fov) / 2) -- focal length
   
-- | creates an environmental camera using the specified parameters
mkEnvironmentCamera
   :: Transform -- ^ the camera to world transform
   -> Flt -- ^ film x resolution
   -> Flt -- ^ film y resolution
   -> Camera
mkEnvironmentCamera = Environment

