
module Graphics.Bling.Camera (

   -- * Creating Cameras
   
   Camera, mkPerspectiveCamera, mkEnvironmentCamera,
   
   -- * Using Cameras
   
   fireRay, CameraSampleResult(..), sampleCam
   ) where

import Control.Monad (liftM)
import Data.Monoid
import qualified Text.PrettyPrint as PP

import Graphics.Bling.Math
import Graphics.Bling.Montecarlo
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

-- | a @Camera@
data Camera
      = ProjectiveCamera {
         _cam2world     :: !Transform,
         _raster2cam    :: !Transform,
         _world2raster  :: !Transform,
         _pixelArea     :: {-# UNPACK #-} !Float, -- the area of a pixel
         _lensRadius    :: {-# UNPACK #-} !Float,
         _focalDistance :: {-# UNPACK #-} !Float }
      | Environment !Transform {-# UNPACK #-} !Float {-# UNPACK #-} !Float -- cam2world xres yres
      deriving (Show)
      
instance Printable Camera where
   prettyPrint (ProjectiveCamera _ _ w2r ap lr fd) = PP.vcat [
      PP.text "Projective" PP.<+> PP.text (show w2r),
      PP.text "Lens Radius" PP.<+> PP.float lr,
      PP.text "Focal Distance" PP.<+>  PP.float fd,
      PP.text "area of single pixel" PP.<+> PP.float ap ]

   prettyPrint (Environment _ _ _) = PP.text "Environment"

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
   , csImgX          :: Float -- ^ pixel pos x
   , csImgY          :: Float -- ^ pixel pos y
   , csPdf           :: Float
   }

sampleCam
   :: Camera -- ^ the camera to sample
   -> Point -- ^ the point in world space
   -> CameraSampleResult

sampleCam (ProjectiveCamera c2w r2c w2r ap _ _) p = smp where
   smp = CameraSampleResult white pLens px py (ap {- * cost3-})
   pRas@(Vector px py _) = transPoint w2r p
   pLens = transPoint c2w $ mkPoint' 0 0 0
   pRas' = transPoint r2c pRas
   cost = abs $ vz (normalize $ transVector (inverse c2w) pRas')
   cost3 = cost * cost * cost
   
sampleCam c _ = error $ "can not sample " ++ show c

--
-- creating cameras
--

mkProjective
   :: Transform -- ^ camera to world
   -> Transform -- ^ the projection
   -> Float -- ^ lens radius
   -> Float -- ^ focal distance
   -> Float -- ^ frame size in x
   -> Float -- ^ frame size in y
   -> Float -- ^ focal length
   -> Camera
   
mkProjective c2w p lr fd sx sy fl = ProjectiveCamera c2w r2c w2r ap lr fd where
   s2r = t <> st2 <> st1
   aspect = sx / sy
   (s0, s1, s2, s3) = if aspect > 1
                         then (-aspect, aspect, -1, 1)
                         else (-1, 1, -1 / aspect, 1 / aspect)
   st1 = scale (Vector sx sy 1)
   st2 = scale (Vector (1 / (s1 - s0)) (1 / (s2 - s3)) 1)
   t = translate (Vector (-s0) (-s3) 0)
   r2s = inverse s2r
   r2c = r2s <> inverse p
   w2r = w2s <> s2r -- world to raster
   w2s = (inverse c2w) <> p  -- world to screen
   ap = (pw * ph) -- pixel area
   pw = fl * (s1 - s0) / sx
   ph = fl * (s3 - s2) / sy
   
-- | creates a perspective camera using the specified parameters
mkPerspectiveCamera
   :: Transform -- ^ the camera to world transform
   -> Float -- ^ lens radius
   -> Float -- ^ focal distance
   -> Float -- ^ field of view in degrees
   -> Float -- ^ frame size in x
   -> Float -- ^ frame size in y
   -> Camera
mkPerspectiveCamera c2w lr fd fov sx sy = mkProjective c2w p lr fd sx sy fl
   where
      p = perspective fov 1e-2 1000
      fl = tan ((radians fov) / 2) -- focal length
   
-- | creates an environmental camera using the specified parameters
mkEnvironmentCamera
   :: Transform -- ^ the camera to world transform
   -> Float -- ^ film x resolution
   -> Float -- ^ film y resolution
   -> Camera
mkEnvironmentCamera = Environment

