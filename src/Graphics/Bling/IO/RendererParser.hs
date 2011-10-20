
module Graphics.Bling.IO.RendererParser (
   defaultRenderer, pRenderer,
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Integrator.LightTracer
import Graphics.Bling.Integrator.Metropolis
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.IntegratorParser

defaultRenderer :: AnyRenderer
defaultRenderer = mkAnyRenderer r where
   r = mkSamplerRenderer defaultSampler defaultSurfaceIntegrator
   
defaultSampler :: Sampler
-- defaultSampler = mkAnySampler $ mkRandomSampler 2
defaultSampler = mkStratifiedSampler 2 2

pRenderer :: JobParser ()
pRenderer = pBlock $ do
   tName <- pString
   ws
   r <- case tName of

             "light" -> do
               pc <- namedInt "passCount"
               pp <- ws >> namedInt "passPhotons"
               return $ mkAnyRenderer $ mkLightTracer pc pp
  
             "metropolis" -> do
                md <- namedInt "maxDepth"
                mpp <- ws >> namedFloat "mpp"
                nboot <- ws >> namedInt "bootstrap"
                plarge <- ws >> namedFloat "plarge"
                dspp <- ws >> namedInt "directSamples"
                return $ mkAnyRenderer $ mkMLT md mpp nboot plarge dspp
                
             "sampler" -> do
                sr <- pSamplerRenderer
                return $ mkAnyRenderer sr
                
             _ -> fail ("unknown renderer type " ++ tName)
             
   s <- getState
   setState s { renderer = r }
   
pSamplerRenderer :: JobParser SamplerRenderer
pSamplerRenderer = (flip namedBlock) "sampled" $ do
   s <- pSampler
   i <- ws >> pSurfaceIntegrator
   return $ mkSamplerRenderer s i
   
pSampler :: JobParser Sampler
pSampler = (flip namedBlock) "sampler" $ do
   t <- many1 alphaNum
   
   case t of  
             "stratified" -> do
                nx <- ws >> namedInt "xSamples"
                ny <- ws >> namedInt "ySamples"
                return $ mkStratifiedSampler nx ny
                
             "random" -> do
                ns <- ws >> namedInt "samples"
                return $ mkRandomSampler ns
                
             _ -> fail $ "unknown sampler type " ++ t
   
