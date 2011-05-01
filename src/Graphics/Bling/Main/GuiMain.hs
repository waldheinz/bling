

import Control.Monad.ST
import Graphics.UI.SDL as SDL
import System (getArgs)

import Graphics.Bling.Image
import Graphics.Bling.Rendering
import Graphics.Bling.IO.RenderJob

data AppConfig = AppConfig {
    screen :: Surface
}

initEnv :: Job -> IO AppConfig
initEnv job = do
   let sizeX = imageSizeX job
   let sizeY = imageSizeY job
   
   s <- setVideoMode sizeX sizeY 32 [SWSurface]
   return $ AppConfig s

waitQuit :: IO ()
waitQuit = waitEvent >>= \evt -> case evt of
                              Quit -> return ()
                              _ -> waitQuit

main :: IO ()
main = SDL.withInit [InitVideo] $ do
   fName <- fmap head getArgs
   job <- fmap parseJob $ readFile fName
   env <- initEnv job
   waitQuit
   
   
   
   