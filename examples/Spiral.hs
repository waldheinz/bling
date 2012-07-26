
import Control.Applicative

import Graphics.Bling.Camera
import Graphics.Bling.Edsl
import Graphics.Bling.IO.RGBE
import Graphics.Bling.Light
import Graphics.Bling.Material.Lafortune
import Graphics.Bling.Material.Metal
import Graphics.Bling.Material.Plastic
import Graphics.Bling.Reflection
import Graphics.Bling.Shape
import Graphics.Bling.Spectrum
import Graphics.Bling.Texture

gold :: Material
gold = mkMetal eta k $ const 0.002 where
   eta = const $ fromSpd $ mkSpd [
      (298.757050, 1.795000), (302.400421, 1.812000), (306.133759, 1.822625),
      (309.960449, 1.830000), (313.884003, 1.837125), (317.908142, 1.840000),
      (322.036835, 1.834250), (326.274139, 1.824000), (330.624481, 1.812000),
      (335.092377, 1.798000), (339.682678, 1.782000), (344.400482, 1.766000),
      (349.251221, 1.752500), (354.240509, 1.740000), (359.374420, 1.727625),
      (364.659332, 1.716000), (370.102020, 1.705875), (375.709625, 1.696000),
      (381.489777, 1.684750), (387.450562, 1.674000), (393.600555, 1.666000),
      (399.948975, 1.658000), (406.505493, 1.647250), (413.280579, 1.636000),
      (420.285339, 1.628000), (427.531647, 1.616000), (435.032196, 1.596250),
      (442.800629, 1.562000), (450.851562, 1.502125), (459.200653, 1.426000),
      (467.864838, 1.345875), (476.862213, 1.242000), (486.212463, 1.086750),
      (495.936707, 0.916000), (506.057861, 0.754500), (516.600769, 0.608000),
      (527.592224, 0.491750), (539.061646, 0.402000), (551.040771, 0.345500),
      (563.564453, 0.306000), (576.670593, 0.267625), (590.400818, 0.236000),
      (604.800842, 0.212375), (619.920898, 0.194000), (635.816284, 0.177750),
      (652.548279, 0.166000), (670.184753, 0.161000), (688.800964, 0.160000),
      (708.481018, 0.160875), (729.318665, 0.164000), (751.419250, 0.169500),
      (774.901123, 0.176000), (799.897949, 0.181375), (826.561157, 0.188000),
      (855.063293, 0.198125), (885.601257, 0.210000) ]
      
   k = const $ fromSpd $ mkSpd [
      (298.757050, 1.920375), (302.400421, 1.920000), (306.133759, 1.918875),
      (309.960449, 1.916000), (313.884003, 1.911375), (317.908142, 1.904000),
      (322.036835, 1.891375), (326.274139, 1.878000), (330.624481, 1.868250),
      (335.092377, 1.860000), (339.682678, 1.851750), (344.400482, 1.846000),
      (349.251221, 1.845250), (354.240509, 1.848000), (359.374420, 1.852375),
      (364.659332, 1.862000), (370.102020, 1.883000), (375.709625, 1.906000),
      (381.489777, 1.922500), (387.450562, 1.936000), (393.600555, 1.947750),
      (399.948975, 1.956000), (406.505493, 1.959375), (413.280579, 1.958000),
      (420.285339, 1.951375), (427.531647, 1.940000), (435.032196, 1.924500),
      (442.800629, 1.904000), (450.851562, 1.875875), (459.200653, 1.846000),
      (467.864838, 1.814625), (476.862213, 1.796000), (486.212463, 1.797375),
      (495.936707, 1.840000), (506.057861, 1.956500), (516.600769, 2.120000),
      (527.592224, 2.326250), (539.061646, 2.540000), (551.040771, 2.730625),
      (563.564453, 2.880000), (576.670593, 2.940625), (590.400818, 2.970000),
      (604.800842, 3.015000), (619.920898, 3.060000), (635.816284, 3.070000),
      (652.548279, 3.150000), (670.184753, 3.445812), (688.800964, 3.800000),
      (708.481018, 4.087687), (729.318665, 4.357000), (751.419250, 4.610188),
      (774.901123, 4.860000), (799.897949, 5.125813), (826.561157, 5.390000),
      (855.063293, 5.631250), (885.601257, 5.880000) ]
   
plexi :: Material
plexi = mkPlastic (const $ fromRGB' 0.01 0.01 0.01) (const $ fromRGB' 0.8 0.8 0.8) (const $ 0.0002)

spiral = render $ do
   let
      w = 576
      h = 1024
   
   env <- parseRGBE <$> readFileBS "/home/trem/Arbeitsplatz/Meins/bling-scenes/envmaps/grace-new.hdr"

   case env of
      (Left e) -> error e
      (Right i) -> add $ mkInfiniteAreaLight (rgbeToTextureMap i) $ rotateX (-90)
   
   setTransform $ lookAt (mkPoint' 0 0 (-10)) (mkPoint' 0 0 0) (mkV' 0 1 0)
   setCamera $ mkPerspectiveCamera (lookAt (mkPoint' 0 17 (-16)) (mkPoint' 0 5.8 0) (mkV' 0 1 0)) 0 1 40
         (fromIntegral w) (fromIntegral h)
   setImageSize (w, h)
   
   setMaterial plexi -- measuredMaterial Clay
   setTransform $ rotateX 90
   shape (mkQuad 500 500) >>= add
   
   let
      bases = take 30 $ map fst $ iterate (\(t, s) -> let s' = s - 0.006 in
         (concatTrans (concatTrans (concatTrans (rotateY 95) (scale $ vpromote s')) (translate $ mkV' 0 1 0)) t, s')) (translate $ mkV (0, 0.5, 0), 1)
      
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
         
   setMaterial gold -- $ measuredMaterial BrushedMetal
   mapM_ bars bases
   
--    (R.mkAnyRenderer $ R.mkSamplerRenderer (mkStratifiedSampler 8 8) (I.mkAnySurface $ mkPathIntegrator 5 3))
   
main :: IO ()
main = spiral

