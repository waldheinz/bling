
module Graphics.Bling.Main.GuiMain where

import Control.Monad
import Foreign
import Graphics.UI.SDL as SDL
import System.Environment (getArgs)
import System.IO
import Text.Printf

import Graphics.Bling.Image
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.IO.RenderJob

data AppConfig = AppConfig
    { screen :: Surface
    , _buff :: Surface
--   appImg :: Image IO
}

initEnv :: RenderJob -> IO AppConfig
initEnv j = do
   s <- setVideoMode w h 32 [SWSurface]
   b <- createRGBSurfaceEndian [] w h 32
   return $ AppConfig s b
   where
      w = imageSizeX j
      h = imageSizeY j

waitQuit :: IO ()
waitQuit = waitEvent >>= \evt -> case evt of
                              Quit -> return ()
                              _ -> waitQuit

lookQuit :: IO Bool
lookQuit = do
   evt <- pollEvent
   case evt of
        Quit -> return False
        NoEvent -> return True
        _ -> lookQuit

prog :: AppConfig -> ProgressReporter
prog ac (SamplesAdded w img) = do
   let s = screen ac
   forM_ (rgbPixels img 1 w) $ putPixel s -- TODO: splat weight ?!
   SDL.flip s
   lookQuit

prog ac (RegionStarted w) = do
   let s = screen ac
   let ps = map (\p -> (p, (0, 255, 255))) $ coverWindow w
   mapM_ (putPixel s) ps
   SDL.flip s
   lookQuit

prog ac (PassDone p img spw) = do
   let w = imageWindow' img
   let s = screen ac
   let ps = rgbPixels img spw w
   mapM_ (putPixel s) ps
   SDL.flip s
   
   putStrLn $ "\nWriting " ++ fname ++ "..."
   h1 <- openFile (fname ++ ".ppm") WriteMode
   writePpm img spw h1
   hClose h1
   
   h2 <- openFile (fname ++ ".hdr") WriteMode
   writeRgbe img spw h2
   hClose h2
   lookQuit
   
   where
         fname = "pass-" ++ printf "%05d" p

prog _ _ = return True

putPixel :: Surface -> ((Int, Int), (Int, Int, Int))-> IO ()
putPixel s ((x, y), (r,g,b))
   | x < 0 || y < 0 = return ()
   | x >= surfaceGetWidth s || y >= surfaceGetHeight s = return ()
   | otherwise = do
      pixels <- castPtr `liftM` surfaceGetPixels s
      (Pixel p) <- mapRGB (surfaceGetPixelFormat s) (fromIntegral r) (fromIntegral g) (fromIntegral b)
      pokeElemOff pixels ((y * surfaceGetWidth s) + x) p
   
--renderWithPreview :: RenderJob -> IO ()
renderWithPreview j r = initEnv j >>= (\e -> render r j $ prog e) >> waitQuit
   
main :: IO ()
main = SDL.withInit [InitEverything] $ do
   fname <- fmap head getArgs
   parseJob fname >>= \ result ->
      case result of
           (Left e) -> putStrLn $ show e
           (Right (job, renderer)) -> do
              env <- initEnv job
              render renderer job $ prog env
              waitQuit
   
