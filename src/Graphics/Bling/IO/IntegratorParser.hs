
module Graphics.Bling.IO.IntegratorParser (
   pSurfaceIntegrator, defaultSurfaceIntegrator
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.BidirPath
import Graphics.Bling.Integrator.Debug
import Graphics.Bling.Integrator.DirectLighting
import Graphics.Bling.Integrator.Path
import Graphics.Bling.IO.ParserCore

defaultSurfaceIntegrator :: AnySurfaceIntegrator
defaultSurfaceIntegrator = mkAnySurface $ mkPathIntegrator 5

pSurfaceIntegrator :: JobParser AnySurfaceIntegrator
pSurfaceIntegrator = (flip namedBlock) "integrator" $ do
   t <- many1 alphaNum
   
   case t of
        
        "bidir" -> do
           return $ mkAnySurface $ mkBidirPathIntegrator

        "debug" -> do
           return $ mkAnySurface $ mkKdVision
           
        "directLighting" -> do
           return $ mkAnySurface $ mkDirectLightingIntegrator False
        
        "path" -> do
           md <- ws >> namedInt "maxDepth"
           return $ mkAnySurface $ mkPathIntegrator md
           
        _ -> fail $ "unknown integrator type " ++ t
