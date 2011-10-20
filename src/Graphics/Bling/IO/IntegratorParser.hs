
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
defaultSurfaceIntegrator = mkAnySurface $ mkPathIntegrator 7 3

pSurfaceIntegrator :: JobParser AnySurfaceIntegrator
pSurfaceIntegrator = (flip namedBlock) "integrator" $ do
   t <- many1 alphaNum
   
   case t of
        
        "bidir" -> do
           md <- ws >> namedInt "maxDepth"
           sd <- ws >> namedInt "sampleDepth"
           return $ mkAnySurface $ mkBidirPathIntegrator md sd

        "debug" -> do
           dt <- ws >> pString
           case dt of
                "kdtree"   -> return $ mkAnySurface $ mkKdVision
                "normals"  -> return $ mkAnySurface $ mkNormalMap
                "reference" -> return $ mkAnySurface $ mkReference
                _          -> fail $ "unknown debug integrator " ++ dt
                
        "directLighting" -> do
           md <- ws >> namedInt "maxDepth"
           return $ mkAnySurface $ mkDirectLightingIntegrator md

        "path" -> do
           md <- ws >> namedInt "maxDepth"
           sd <- ws >> namedInt "sampleDepth"
           return $ mkAnySurface $ mkPathIntegrator md sd
           
        _ -> fail $ "unknown integrator type " ++ t
