
module Graphics.Bling.IO.SamplerParser (
   defaultSampler
   ) where

import Graphics.Bling.Sampling
import Graphics.Bling.Sampler.Random
import Graphics.Bling.Sampler.Stratified

defaultSampler :: AnySampler
defaultSampler = mkAnySampler $ mkRandomSampler 2
