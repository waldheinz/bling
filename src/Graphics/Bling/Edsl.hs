
module Graphics.Bling.Edsl (
   module Data.Monoid,
   module Graphics.Bling.Camera,
   module Graphics.Bling.Gui,
   module Graphics.Bling.IO.RGBE,
   module Graphics.Bling.Image,
   module Graphics.Bling.Integrator.BidirPath,
   module Graphics.Bling.Integrator.Path,
   module Graphics.Bling.Light,
   module Graphics.Bling.Material,
   module Graphics.Bling.Primitive.Fractal,
   module Graphics.Bling.Rendering,
   module Graphics.Bling.Reflection,
   module Graphics.Bling.Sampling,
   module Graphics.Bling.Shape,
   module Graphics.Bling.Spectrum,
   module Graphics.Bling.SunSky,
   module Graphics.Bling.Texture,
   
   buildJob, emit, CanAdd(..), shape, setTransform, setCamera, setMaterial,
   setImageSize, readFileBS, setFilter, io
   ) where

import Control.Applicative
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.Monoid

import Graphics.Bling.Camera
import Graphics.Bling.Gui
import Graphics.Bling.Image
import Graphics.Bling.Integrator.BidirPath
import Graphics.Bling.Integrator.Path
import Graphics.Bling.Light
import Graphics.Bling.IO.RGBE
import Graphics.Bling.Material
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.Fractal
import Graphics.Bling.Primitive.Geometry
import Graphics.Bling.Reflection
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum
import Graphics.Bling.SunSky
import Graphics.Bling.Texture

data MyState = MyState
   { prims     :: [AnyPrim]
   , lights    :: [Light]
   , imgSize   :: PixelSize
   , pfilter   :: Filter
   , camera    :: Camera
   , transform :: Transform
   , material  :: Material
   , emission  :: Maybe Spectrum
   , geomId    :: Int
   }

initialState :: MyState
initialState = MyState [] [] (640, 360) mkBoxFilter
   (mkPerspectiveCamera (lookAt (mkPoint' 0 5 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)) 0 1 90 640 360)

   mempty (mkMatte (const $ fromRGB' 0.9 0.9 0.9) (const 0)) Nothing 0

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

instance CanAdd AnyPrim where
   add p = modify $ \s -> s { prims = p : prims s }

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

setFilter :: Filter -> DslState ()
setFilter f = modify $ \s -> s { pfilter = f }

io :: IO a -> DslState a
io = liftIO

-- | reads a file into a lazy ByteString
readFileBS :: FilePath -> DslState BS.ByteString
readFileBS n = liftIO $ do
   putStrLn $ "Reading " ++ n
   BS.readFile n

buildJob :: DslState a -> IO RenderJob
buildJob f = do
   (MyState ps ls sz filt cam _ _ _ _) <- execStateT f initialState
   return $ mkJob (mkScene ls ps cam) filt sz

