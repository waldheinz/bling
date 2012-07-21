
import Graphics.UI.SDL as SDL
import System.Environment (getArgs)

import Graphics.Bling.Gui
import Graphics.Bling.IO.RenderJob

main :: IO ()
main = SDL.withInit [InitEverything] $ do
   fname <- fmap head getArgs
   parseJob fname >>= \ result ->
      case result of
           (Left e) -> putStrLn $ show e
           (Right (job, renderer)) -> renderWithPreview job renderer
   
