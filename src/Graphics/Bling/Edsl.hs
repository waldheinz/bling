
module Graphics.Bling.Edsl (
   render, emit, CanAdd(..), shape, setTransform, setCamera, setMaterial, setImageSize
   ) where

import Control.Applicative
import Control.Monad.State

import Graphics.Bling.Camera
import Graphics.Bling.Filter
import Graphics.Bling.Gui
import qualified Graphics.Bling.Integrator as I
import Graphics.Bling.Integrator.Path
import Graphics.Bling.Light
import Graphics.Bling.Material.Matte
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.Geometry
import Graphics.Bling.Reflection
import qualified Graphics.Bling.Rendering as R
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum
import Graphics.Bling.Scene
import Graphics.Bling.Shape
import Graphics.Bling.Texture
import Graphics.Bling.Transform

data MyState = MyState
   { prims        :: [AnyPrim]
   , lights       :: [Light]
   , imgSize      :: (Int, Int)
   , pixelFilter  :: Filter
   , camera       :: Camera
   , renderer     :: R.AnyRenderer
   , transform    :: Transform
   , material     :: Material
   , emission     :: Maybe Spectrum
   , geomId       :: Int
   }

initialState :: MyState
initialState = MyState [] [] (640, 360) mkBoxFilter
   (mkPerspectiveCamera (lookAt (mkPoint' 0 5 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)) 0 1 90 640 360)
   (R.mkAnyRenderer $ R.mkSamplerRenderer (mkRandomSampler 4) (I.mkAnySurface $ mkPathIntegrator 5 3))
   identity (mkMatte (constant $ fromRGB' 0.9 0.9 0.9) (constant 0)) Nothing 0

type DslState a = (State MyState a)

class CanAdd a where
   add :: a -> DslState ()

-- | Geometry without id
newtype Geometry' = G' { unG' :: (Int -> Geometry) }

nextId :: DslState Int
nextId = get >>= \s -> let i' = geomId s + 1 in do
   put s { geomId = i' }
   return i'

shape :: Shape -> DslState Geometry'
shape s = let f = (\t m e -> mkGeom t False m e s)
              in G' <$>(f <$> (gets transform) <*> (gets material) <*> (gets emission))

instance CanAdd Geometry' where
   add g = unG' g <$> nextId >>= \g' -> modify (\s -> s { prims = (mkAnyPrim g') : (prims s) })

emit :: Spectrum -> DslState ()
emit s = let s' = if isBlack s then Nothing else Just s in modify (\st -> st { emission = s' })

setTransform :: Transform -> DslState ()
setTransform t = modify $ \s -> s { transform = t }

setCamera :: Camera -> DslState ()
setCamera c = modify $ \s -> s { camera = c }

setImageSize :: (Int, Int) -> DslState ()
setImageSize sz = modify $ \s -> s { imgSize = sz }

setMaterial :: Material -> DslState ()
setMaterial m = modify $ \s -> s { material = m }

render :: State MyState () -> IO ()
render f = do
   let
      (MyState ps ls (sx, sy) filt cam renderer _ _ _ _) = execState f initialState
      scene = mkScene ls ps cam
      job = R.mkJob scene filt sx sy
      
   renderWithPreview job renderer
   return ()

