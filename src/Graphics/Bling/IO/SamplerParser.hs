
module Graphics.Bling.IO.SamplerParser (
   defaultSampler, pSampler
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.IO.ParserCore
import Graphics.Bling.Sampling
import Graphics.Bling.Sampler.Random
import Graphics.Bling.Sampler.Stratified

defaultSampler :: AnySampler
-- defaultSampler = mkAnySampler $ mkRandomSampler 2
defaultSampler = mkAnySampler $ mkStratifiedSampler 2 2

pSampler :: JobParser ()
pSampler = (flip namedBlock) "sampler" $ do
   t <- many1 alphaNum
   
   smp <- case t of  
             "stratified" -> do
                nx <- ws >> namedInt "xSamples"
                ny <- ws >> namedInt "ySamples"
                return $ mkAnySampler $ mkStratifiedSampler nx ny
                
             "random" -> do
                ns <- ws >> namedInt "samples"
                return $ mkAnySampler $ mkRandomSampler ns
                
             _ -> fail $ "unknown sampler type " ++ t

   s <- getState
   setState s { sampler = smp }
   