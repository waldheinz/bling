
module Examples (
  staple, onePixel
  ) where

import Control.Monad ( forM_ )

import Graphics.Bling

onePixel :: IO ()
onePixel = do
  imgShow <- execImageT (addSample' 128 128 white) $ mkImage (mkGaussFilter 8 8 1) (256, 256)
  writePng imgShow $ "one-pixel.png"

staple :: IO ()
staple = do
  let
    flts =
      [ ("box"     , mkBoxFilter)
      , ("gauss"   , mkGaussFilter 2 2 1)
      , ("mitchell", mkMitchellFilter 2 2 (1/3) (1/3))
      , ("sinc"    , mkSincFilter 2 2 3)
      , ("triangle", mkTriangleFilter 2 2)
      ]
      
    width = 640
    height = 360
    offs = [0, 0.2, 0.4, 0.6, 0.8]
    
    fltTest = do
      pixels <- fmap coverWindow sampleExtent'
      forM_ [(px, sx, sy) | px <-pixels, sx <- offs, sy <- offs] $
        \((px, py), sx, sy) -> do
          let
            (x, y)   = (fromIntegral px + sx, fromIntegral py + sy) -- shift to center of pixel
            sz = min (fromIntegral width) (fromIntegral height)
            (dx, dy) = (x / sz - 0.5, 0.9 - y / sz)
            d = (sin $ (500 * (dx * dx + dy * dy)))
            
          addSample' x y $ rgbToSpectrumIllum (d, d, d)

    showSize = 256
  
    fltShow flt = do
      pixels <- fmap coverWindow sampleExtent'
      
      forM_ pixels $ \(px, py) -> do
        let
          (x, y)   = (fromIntegral px, fromIntegral py)
--          (fw, fh) = filterSize flt
          (x', y') = ((x / showSize - 0.5) * 3, (y / showSize - 0.5) * 3)
          d = evalFilter flt x' y'
          c = if d > 0 then (d, 0, 0) else (0, -d, 0)
          
        addSample' x y $ rgbToSpectrumIllum c
  
  forM_ flts $ \(fname, flt) -> do
    putStrLn fname
    
    imgShow <- execImageT (fltShow flt) $ mkImage mkBoxFilter (256, 256)
    writePng imgShow $ "filter-show-" ++ fname ++ ".png"
    
    imgTest <- execImageT fltTest $ mkImage flt (width, height)
    writePng imgTest $ "filter-test-" ++ fname ++ ".png"
    
    
    
    
