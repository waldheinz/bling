
module Examples (
  imageFilters
  ) where

import Control.Monad ( forM_ )

import Graphics.Bling

imageFilters :: IO ()
imageFilters = do
  let
    flts =
      [ ("box"     , mkBoxFilter)
      , ("gauss"   , mkGaussFilter 3 3 2)
      , ("mitchell", mkMitchellFilter 3 3 (1/3) (1/3))
      , ("sinc"    , mkSincFilter 3 3 3)
      , ("triangle", mkTriangleFilter 3 3)
      ]
      
    width = 480
    height = 270
    offs = [0, 0.2, 0.4, 0.6, 0.8]
    
    fltTest = do
      pixels <- fmap coverWindow sampleExtent'
      forM_ [(px, sx, sy) | px <-pixels, sx <- offs, sy <- offs] $
        \((px, py), sx, sy) -> do
          let
            (x, y)   = (fromIntegral px + sx, fromIntegral py + sy) -- shift to center of pixel
            sz = min (fromIntegral width) (fromIntegral height)
            (dx, dy) = (x / sz - 0.5, 1 - y / sz)
            d = abs (sin $ (150 * (dx * dx + dy * dy)))
            
          addSample' x y $ rgbToSpectrumIllum (d, d, d)

    showSize = 64
  
    fltShow flt = do
      pixels <- fmap coverWindow sampleExtent'
      
      forM_ pixels $ \(px, py) -> do
        let
          (x, y)   = (fromIntegral px, fromIntegral py)
          (x', y') = ((x / fromIntegral showSize - 0.5) * 4, (y / fromIntegral showSize - 0.5) * 4)
          d = evalFilter flt x' y'
          c = if d > 0 then (d, 0, 0) else (0, -d, 0)
          
        addSample' x y $ rgbToSpectrumIllum c
  
  forM_ flts $ \(fname, flt) -> do
    putStrLn fname
    
    imgShow <- execImageT (fltShow flt) $ mkImage mkBoxFilter (showSize, showSize)
    writePng imgShow $ "filter-show-" ++ fname ++ ".png"
    
    imgTest <- execImageT fltTest $ mkImage flt (width, height)
    writePng imgTest $ "filter-test-" ++ fname ++ ".png"
    
    
    
    
