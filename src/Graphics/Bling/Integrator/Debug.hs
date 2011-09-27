

module Graphics.Bling.Integrator.Debug (
   DebugIntegrator, mkKdVision, mkNormalMap, mkReference
   ) where


import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Montecarlo
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.KdTree

import Control.Monad (liftM)
import Data.Maybe
import qualified Text.PrettyPrint as PP

--------------------------------------------------------------------------------
-- kd-tree vision integrator
--------------------------------------------------------------------------------

data DebugIntegrator
   = KdTreeVis
   | NormalMap
   | Reference 

mkKdVision :: DebugIntegrator
mkKdVision = KdTreeVis

mkNormalMap :: DebugIntegrator
mkNormalMap = NormalMap

mkReference :: DebugIntegrator
mkReference = Reference

instance Printable DebugIntegrator where
   prettyPrint KdTreeVis = PP.text "kd tree vision"
   prettyPrint NormalMap = PP.text "Normal Map"
   prettyPrint Reference = PP.text "Reference"

instance SurfaceIntegrator DebugIntegrator where
   sampleCount1D _ = 0
   sampleCount2D _ = 0
   
   contrib KdTreeVis scene tell r = do
      let stats = dbgTraverse t r
      let ints = fromIntegral $ intersections stats
      li <- mkContrib (1, fromRGB (fromIntegral $ nodesTraversed stats, ints, 0)) False
      liftSampled $ tell $ li
      where
         t = scenePrim scene

   contrib NormalMap scene tell ray = do
      if isJust mint
         then mkContrib (intToSpectrum (fromJust mint)) False >>= (liftSampled . tell)
         else return ()

      where
         mint = scene `intersect` ray
      
   contrib Reference scene tell ray = go ray >>= \ws -> mkContrib ws False >>= (liftSampled . tell) where
      go r = maybe (return (0, black)) evalInt (scene `intersect` r) where
          evalInt int = let
                            dg = intGeometry int
                            p = dgP dg
                            n = dgN dg
                            wo = -(rayDir r)
                            le = intLe int wo
                            bsdf = intBsdf int
                        in do
                           wi <- uniformSampleSphere `liftM` rnd2D
                           (_, rest) <- go (Ray p wi epsilon infinity)
                           return $ (1, le + sScale (evalBsdf bsdf wo wi * rest) (absDot wi n * 4 * pi))
         
intToSpectrum :: Intersection -> WeightedSpectrum
intToSpectrum int = (1, fromRGB (r, g, b)) where
   Vector r g b = (vpromote 1 + n) / 2
   n = dgN $ intGeometry int
      