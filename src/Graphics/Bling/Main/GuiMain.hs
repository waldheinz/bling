
import Control.Concurrent
import Control.Monad.ST
import Control.Monad
import Foreign
import Graphics.UI.SDL as SDL
import System (getArgs)

import Graphics.Bling.Image
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.IO.RenderJob

data AppConfig = AppConfig {
    screen :: Surface,
    _buff :: Surface
}

initEnv :: Job -> IO AppConfig
initEnv job = do
   let w = imageSizeX job
   let h = imageSizeY job
   
   s <- setVideoMode w h 32 [SWSurface]
   b <- createRGBSurfaceEndian [] w h 32
   return $ AppConfig s b

waitQuit :: IO ()
waitQuit = waitEvent >>= \evt -> case evt of
                              Quit -> return ()
                              _ -> waitQuit

prog :: AppConfig -> ProgressReporter
prog ac (Progress (SamplesAdded w) img) = do
   let s = screen ac
   ps <- stToIO $ rgbPixels img w
   mapM_ (putPixel s) ps
   SDL.flip s
   evt <- pollEvent
   return $ case evt of
        Quit -> False
        _ -> True
   
prog _ _ = return True

putPixel :: Surface -> ((Int, Int), (Int, Int, Int))-> IO ()
putPixel s ((x, y), (r,g,b)) = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    (Pixel p) <- mapRGB (surfaceGetPixelFormat s) (fromIntegral r) (fromIntegral g) (fromIntegral b)
    pokeElemOff pixels ((y * surfaceGetWidth s) + x) p
   
main :: IO ()
main = SDL.withInit [InitEverything] $ do
   fName <- fmap head getArgs
   job <- fmap parseJob $ readFile fName
   env <- initEnv job
   render job (prog env)
   waitQuit
   