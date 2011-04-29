

import Control.Monad.ST
import Graphics.UI.SDL as SDL
import System (getArgs)

import Graphics.Bling.Camera
import Graphics.Bling.Image
import Graphics.Bling.Integrator
import Graphics.Bling.Random
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.IO.RenderJob

data AppConfig = AppConfig {
    screen :: Surface
}

initEnv :: Job -> IO AppConfig
initEnv job = do
   let sizeX = imageSizeX job
   let sizeY = imageSizeY job
   
   screen <- setVideoMode sizeX sizeY 32 [SWSurface]
   return $ AppConfig screen

waitQuit :: IO ()
waitQuit = waitEvent >>= \evt -> case evt of
                              Quit -> return ()
                              _ -> waitQuit

main :: IO ()
main = SDL.withInit [InitVideo] $ do
   
   args <- getArgs
   let fName = head args
   job <- fmap parseJob $ readFile fName
   env <- initEnv job
   let sizeX = imageSizeX job
   let sizeY = imageSizeY job
   img <- stToIO $ mkImage (jobPixelFilter job) sizeX sizeY
   waitQuit
   
   
   
   