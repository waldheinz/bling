
module Graphics.Bling.Edsl (
   render, emit, CanAdd(..), shape, setTransform, setCamera, setMaterial,
   setImageSize, readFileBS
   ) where

import Control.Applicative
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS

import Graphics.Bling.Camera
import Graphics.Bling.Gui
import Graphics.Bling.Image
import qualified Graphics.Bling.Integrator as I
import Graphics.Bling.Integrator.Path
import Graphics.Bling.Light
import Graphics.Bling.Material.Matte
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.Geometry
import Graphics.Bling.Reflection
import qualified Graphics.Bling.Rendering as R
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Shape
import Graphics.Bling.Texture

data MyState = MyState
   { prims        :: [AnyPrim]
   , lights       :: [Light]
   , imgSize      :: (Int, Int)
   , _pixelFilter  :: Filter
   , camera       :: Camera
   , _renderer     :: R.AnyRenderer
   , transform    :: Transform
   , material     :: Material
   , emission     :: Maybe Spectrum
   , geomId       :: Int
   }

initialState :: MyState
initialState = MyState [] [] (640, 360) mkBoxFilter
   (mkPerspectiveCamera (lookAt (mkPoint' 0 5 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)) 0 1 90 640 360)
   (R.mkAnyRenderer $ R.mkSamplerRenderer (mkStratifiedSampler 8 8) (I.mkAnySurface $ mkPathIntegrator 5 3))
   identity (mkMatte (constant $ fromRGB' 0.9 0.9 0.9) (constant 0)) Nothing 0

type DslState a = (StateT MyState IO a)

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

instance CanAdd Light where
   add l = modify $ \s -> s { lights = l : (lights s) }

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

-- | reads a file into a lazy ByteString
readFileBS :: FilePath -> DslState BS.ByteString
readFileBS n = liftIO $ do
   putStrLn $ "Reading " ++ n
   BS.readFile n

render :: DslState a -> IO ()
render f = do
   (MyState ps ls (sx, sy) filt cam r _ _ _ _) <- execStateT f initialState
   
   let
      scene = mkScene ls ps cam
      job = R.mkJob scene filt sx sy
      
   renderWithPreview job r
   return ()

