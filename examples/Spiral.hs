
module Graphics.Bling.Examples.Spiral where

import Control.Applicative

import Graphics.Bling.Camera
import Graphics.Bling.Edsl
import Graphics.Bling.IO.RGBE
import Graphics.Bling.Light
import Graphics.Bling.Material.Lafortune
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

spiral = render $ do
   let
      w = 720
      h = 1280 
   
   env <- parseRGBE <$> readFileBS "/home/trem/Arbeitsplatz/Meins/bling-scenes/envmaps/grace-new.hdr" -- "envmaps/studio015.hdr"

   case env of
      (Left e) -> error e
      (Right i) -> add $ mkInfiniteAreaLight (rgbeToTextureMap i) $ rotateX (-90)
   
   setTransform $ lookAt (mkPoint' 0 0 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)
   setCamera $ mkPerspectiveCamera (lookAt (mkPoint' 0 17 (-16)) (mkPoint' 0 5.8 0) (mkV' 0 1 0)) 0 1 40 720 1280
   setImageSize (w, h)
   
   setMaterial $ measuredMaterial Clay
   setTransform $ rotateX 90
   shape (mkQuad 500 500) >>= add
   
   let
      bases = take 40 $ map fst $ iterate (\(t, s) -> let s' = 0.993 * s in
         (concatTrans (concatTrans (concatTrans (rotateY 95) (scale $ vpromote s')) (translate $ mkV' 0 1 0)) t, s')) (identity, 1)
      
      bar t = do
         setTransform t
         shape (mkCylinder 0.5 (-5) 5 360) >>= add
         setTransform $ concatTrans (translate $ mkV' 0 0 5) t
         shape (mkSphere 0.5) >>= add
         setTransform $ concatTrans (translate $ mkV' 0 0 (-5)) t
         shape (mkSphere 0.5) >>= add
         
      bars t = do
         bar $ concatTrans (translate $ mkV'   3.5  0 0) t
         bar $ concatTrans (translate $ mkV' (-3.5) 0 0) t
         
   setMaterial $ measuredMaterial BrushedMetal
   mapM_ bars bases
   
main = spiral

