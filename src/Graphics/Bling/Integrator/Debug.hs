

module Graphics.Bling.Integrator.Debug (
   DebugIntegrator, mkKdVision
   ) where


import Graphics.Bling.DifferentialGeometry
import Graphics.Bling.Integrator
import Graphics.Bling.Sampling
import Graphics.Bling.Scene
import Graphics.Bling.Spectrum
import Graphics.Bling.Primitive.KdTree

import qualified Text.PrettyPrint as PP

--------------------------------------------------------------------------------
-- kd-tree vision integrator
--------------------------------------------------------------------------------

data DebugIntegrator = KdTreeVis

mkKdVision :: DebugIntegrator
mkKdVision = KdTreeVis

instance Printable DebugIntegrator where
   prettyPrint _ = PP.text "kd tree vision"

instance SurfaceIntegrator DebugIntegrator where
--   contrib :: (PrimMonad m) => a -> Scene -> Consumer m -> Ray -> Sampled m ()
   sampleCount1D _ = 0
   sampleCount2D _ = 0
   
   contrib KdTreeVis scene tell r = do
      let stats = dbgTraverse t r
      let ints = fromIntegral $ intersections stats
      li <- mkContrib (1, fromRGB (fromIntegral $ nodesTraversed stats, ints, 0))
      liftSampled $ tell $ li
      where
         t = scenePrim scene

