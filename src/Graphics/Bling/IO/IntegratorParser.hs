
module Graphics.Bling.IO.IntegratorParser (
   pSurfaceIntegrator, defaultSurfaceIntegrator
   ) where

import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.BidirPath
import Graphics.Bling.Integrator.Debug
import Graphics.Bling.Integrator.DirectLighting
import Graphics.Bling.Integrator.Path
import Graphics.Bling.IO.ParserCore

defaultSurfaceIntegrator :: AnySurfaceIntegrator
defaultSurfaceIntegrator = mkAnySurface $ mkPathIntegrator 7 3

pSurfaceIntegrator :: JobParser AnySurfaceIntegrator
pSurfaceIntegrator = (flip namedBlock) "integrator" $ 
   pString >>= \t -> case t of
        "bidir" -> do
           md <- namedInt "maxDepth"
           sd <- namedInt "sampleDepth"
           return $ mkAnySurface $ mkBidirPathIntegrator md sd

        "bidirnod" -> do
           md <- namedInt "maxDepth"
           sd <- namedInt "sampleDepth"
           return $ mkAnySurface $ mkNoDirectBidirIntegrator md sd

        "debug" -> do
           dt <- pString
           case dt of
                "kdtree"   -> return $ mkAnySurface $ mkKdVision
                "normals"  -> return $ mkAnySurface $ mkNormalMap
                "reference" -> return $ mkAnySurface $ mkReference
                _          -> fail $ "unknown debug integrator " ++ dt
                
        "directLighting" -> do
           md <- namedInt "maxDepth"
           return $ mkAnySurface $ mkDirectLightingIntegrator md

        "path" -> do
           md <- namedInt "maxDepth"
           sd <- namedInt "sampleDepth"
           return $ mkAnySurface $ mkPathIntegrator md sd
           
        _ -> fail $ "unknown integrator type " ++ t
        
