
module Graphics.Bling.IO.IntegratorParser (
   pSurfaceIntegrator, defaultSurfaceIntegrator
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.BidirPath
import Graphics.Bling.Integrator.DirectLighting
import Graphics.Bling.Integrator.Path
import Graphics.Bling.IO.ParserCore

defaultSurfaceIntegrator :: AnySurfaceIntegrator
defaultSurfaceIntegrator = mkAnySurface $ mkPathIntegrator 20

pSurfaceIntegrator :: JobParser ()
pSurfaceIntegrator = (flip namedBlock) "integrator" $ do
   t <- many1 alphaNum
   
   i <- case t of
        "bidir" -> do
           return $ mkAnySurface $ mkBidirPathIntegrator
           
        "directLighting" -> do
           return $ mkAnySurface $ mkDirectLightingIntegrator False
        
        "path" -> do
           md <- ws >> namedInt "maxDepth"
           return $ mkAnySurface $ mkPathIntegrator md
           
        _ -> fail $ "unknown integrator type " ++ t

   s <- getState
   setState s { surfaceIntegrator = i }