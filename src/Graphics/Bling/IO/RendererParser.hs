
module Graphics.Bling.IO.RendererParser (
   defaultRenderer, pRenderer,
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.Sampler.Random
import Graphics.Bling.Sampler.Stratified
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.IntegratorParser

defaultRenderer :: AnyRenderer
defaultRenderer = mkAnyRenderer r where
   r = mkSamplerRenderer defaultSampler defaultSurfaceIntegrator
   
defaultSampler :: AnySampler
-- defaultSampler = mkAnySampler $ mkRandomSampler 2
defaultSampler = mkAnySampler $ mkStratifiedSampler 2 2

pRenderer :: JobParser ()
pRenderer = (flip namedBlock) "renderer" $ do
   tName <- many1 alphaNum
   ws
   r <- case tName of
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
   
pSampler :: JobParser AnySampler
pSampler = (flip namedBlock) "sampler" $ do
   t <- many1 alphaNum
   
   case t of  
             "stratified" -> do
                nx <- ws >> namedInt "xSamples"
                ny <- ws >> namedInt "ySamples"
                return $ mkAnySampler $ mkStratifiedSampler nx ny
                
             "random" -> do
                ns <- ws >> namedInt "samples"
                return $ mkAnySampler $ mkRandomSampler ns
                
             _ -> fail $ "unknown sampler type " ++ t
   