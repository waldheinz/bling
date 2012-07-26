
module Graphics.Bling.Gui (
   PreviewWindow, mkPreviewWindow, previewProgress, renderWithPreview
) where

import Control.Monad
import Foreign
import Graphics.UI.SDL as SDL

import Graphics.Bling.Image
import Graphics.Bling.Rendering
import Graphics.Bling.Sampling

data PreviewWindow = AppConfig
    { screen   :: Surface
    , _buff    :: Surface
    }

mkPreviewWindow :: (Int, Int) -> IO PreviewWindow
mkPreviewWindow (w, h) = do
   s <- setVideoMode w h 32 [SWSurface]
   b <- createRGBSurfaceEndian [] w h 32
   return $ AppConfig s b

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

previewProgress :: PreviewWindow -> ProgressReporter
previewProgress ac (SamplesAdded w img) = do
   let s = screen ac
   forM_ (rgbPixels img 1 w) $ putPixel s -- TODO: splat weight ?!
   SDL.flip s
   lookQuit

previewProgress ac (RegionStarted w) = do
   let s = screen ac
   let ps = map (\p -> (p, (0, 255, 255))) $ coverWindow w
   mapM_ (putPixel s) ps
   SDL.flip s
   lookQuit

previewProgress ac (PassDone _ img spw) = do
   let w = imageWindow' img
   let s = screen ac
   let ps = rgbPixels img spw w
   mapM_ (putPixel s) ps
   SDL.flip s
   lookQuit
   
previewProgress _ _ = return True

putPixel :: Surface -> ((Int, Int), (Int, Int, Int))-> IO ()
putPixel s ((x, y), (r,g,b))
   | x < 0 || y < 0 = return ()
   | x >= surfaceGetWidth s || y >= surfaceGetHeight s = return ()
   | otherwise = do
      pixels <- castPtr `liftM` surfaceGetPixels s
      (Pixel p) <- mapRGB (surfaceGetPixelFormat s) (fromIntegral r) (fromIntegral g) (fromIntegral b)
      pokeElemOff pixels ((y * surfaceGetWidth s) + x) p

renderWithPreview :: RenderJob -> AnyRenderer -> IO ()
renderWithPreview j r = do
   wnd <- mkPreviewWindow (imageSizeX j, imageSizeY j)
   render r j $ previewProgress wnd
   waitQuit

