
module Graphics.Bling.IO.IntegratorParser (
   pSurfaceIntegrator, defaultSurfaceIntegrator
   ) where

import Graphics.Bling.Integrator
import Graphics.Bling.Integrator.BidirPath
import Graphics.Bling.Integrator.Debug
import Graphics.Bling.Integrator.DirectLighting
import Graphics.Bling.Integrator.Path
import Graphics.Bling.IO.ParserCore

defaultSurfaceIntegrator :: SurfaceIntegrator
defaultSurfaceIntegrator = mkPathIntegrator 7 3

pSurfaceIntegrator :: JobParser SurfaceIntegrator
pSurfaceIntegrator = (flip namedBlock) "integrator" $ 
   pString >>= \t -> case t of
        "bidir" -> do
           md <- namedInt "maxDepth"
           sd <- namedInt "sampleDepth"
           return $ mkBidirPathIntegrator md sd

        "bidirnod" -> do
           md <- namedInt "maxDepth"
           sd <- namedInt "sampleDepth"
           return $ mkNoDirectBidirIntegrator md sd

        "debug" -> do
           dt <- pString
           case dt of
                "kdtree"   -> return $ mkKdVision
                "normals"  -> return $ mkNormalMap
                "reference" -> return $ mkReference
                _          -> fail $ "unknown debug integrator " ++ dt
                
        "directLighting" -> do
           md <- namedInt "maxDepth"
           return $ mkDirectLightingIntegrator md

        "path" -> do
           md <- namedInt "maxDepth"
           sd <- namedInt "sampleDepth"
           return $ mkPathIntegrator md sd
           
        _ -> fail $ "unknown integrator type " ++ t
        
