
module Graphics.Bling.Edsl where

import Control.Monad.State

import Graphics.Bling.Camera
import Graphics.Bling.Filter
import qualified Graphics.Bling.Integrator as I
import Graphics.Bling.Integrator.Path
import Graphics.Bling.Light
import Graphics.Bling.Main.GuiMain
import Graphics.Bling.Primitive
import qualified Graphics.Bling.Rendering as R
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Transform

data MyState = MyState
   { prims        :: [AnyPrim]
   , lights       :: [Light]
   , imgSize      :: (Int, Int)
   , pixelFilter  :: Filter
   , camera       :: Camera
   , renderer     :: R.AnyRenderer
   }

initialState :: MyState
initialState = MyState [] [] (640, 360) mkBoxFilter
   (mkPerspectiveCamera identity 0 1 90 640 360)
   (R.mkAnyRenderer $ R.mkSamplerRenderer (mkRandomSampler 4) (I.mkAnySurface $ mkPathIntegrator 5 3))

render :: State MyState () -> IO ()
render f = do
   let
      (MyState ps ls (sx, sy) filt cam renderer) = execState f initialState
      scene = mkScene ls ps cam
      job = R.mkJob scene filt sx sy
      
   renderWithPreview job renderer
   return ()

