
module Examples (
  staple
  ) where

import Graphics.Bling

staple :: IO ()
staple = do
  let
    flt = mkBoxFilter
    img = mkImage flt (512, 512)
    moire = mapM
              (\(x, y) -> addSample' x y $ let d = 0.5 in rgbToSpectrumIllum (d, d, d))
              [(x, y) | x <- [0..511], y <- [0..511]]

  img' <- execImageT moire img

  writePng img' "weiss.png"
