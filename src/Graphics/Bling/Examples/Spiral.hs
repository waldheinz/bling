
module Graphics.Bling.Examples.Spiral where

import Graphics.Bling.Camera
import Graphics.Bling.Edsl
import Graphics.Bling.Material.Lafortune
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

spiral = render $ do
   
  -- setTransform $ lookAt (mkPoint' 0 0 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)
   setCamera $ mkPerspectiveCamera (lookAt (mkPoint' 0 13 (-9)) (mkPoint' 0 7 0) (mkV' 0 1 0)) 0 1 70 360 640  
   emit $ sBlackBody 3000
   setTransform $ translate $ mkV' 15 15 (-10)
   shape (mkSphere 1) >>= add
   emit $ sBlackBody 2500
   setTransform $ translate $ mkV' (-15) 10 (-10)
   shape (mkSphere 3) >>= add
   emit black
   
   setTransform $ rotateX 90
   shape (mkQuad 500 500) >>= add
   
   let
   --   a (*$) b = concatTrans a b
      bases = take 35 $ map fst $ iterate (\(t, s) -> let s' = 0.993 * s in
         (concatTrans (concatTrans (concatTrans (rotateY 95) (scale $ vpromote s')) (translate $ mkV' 0 1 0)) t, s')) (identity, 1)
      
      bar t = do
         setTransform t
         shape (mkCylinder 0.5 (-5) 5 360) >>= add
         setTransform $ concatTrans (translate $ mkV' 0 0 5) t
         shape (mkSphere 0.6) >>= add
         setTransform $ concatTrans (translate $ mkV' 0 0 (-5)) t
         shape (mkSphere 0.6) >>= add
         
      bars t = do
         bar $ concatTrans (translate $ mkV'   3.5  0 0) t
         bar $ concatTrans (translate $ mkV' (-3.5) 0 0) t
         
   setMaterial $ measuredMaterial BrushedMetal      
   mapM_ bars bases
   
main = spiral

