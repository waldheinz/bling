
module Graphics.Bling.IO.IntegratorParser (
   pSurfaceIntegrator, defaultSurfaceIntegrator
   ) where

import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.Path
import Graphics.Bling.IO.ParserCore

defaultSurfaceIntegrator :: AnySurfaceIntegrator
defaultSurfaceIntegrator = mkAnySurface $ mkPathIntegrator 20

pSurfaceIntegrator :: JobParser AnySurfaceIntegrator
pSurfaceIntegrator = undefined