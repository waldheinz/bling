
module Graphics.Bling.Examples.Spiral where

import Graphics.Bling.Edsl
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

spiral = render $ do
  -- setTransform $ lookAt (mkPoint' 0 0 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)
   emit $ sBlackBody 3000
   setTransform $ translate $ mkV' 15 15 0
   shape (mkSphere 1) >>= add
   emit black
   
   setTransform $ rotateX 90
   shape (mkQuad 50 50) >>= add
   
   let
      bases = take 15 $ map fst $ iterate (\(t, s) -> let s' = 0.9 * s in
         (concatTrans (concatTrans (scale $ vpromote s') (translate $ mkV' 0 1 0)) t, s')) (identity, 1)
   
   mapM_ (\t -> setTransform t >> shape (mkSphere 0.5) >>= add) bases
   
main = spiral

