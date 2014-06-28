
module Graphics.Bling.IO.RendererParser (
   defaultRenderer, pRenderer,
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Rendering
import Graphics.Bling.Sampling
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.IntegratorParser
import Graphics.Bling.Renderer.LightTracer
import Graphics.Bling.Renderer.Metropolis
import Graphics.Bling.Renderer.SPPM

defaultRenderer :: AnyRenderer
defaultRenderer = mkAnyRenderer r where
   r = mkSamplerRenderer defaultSampler defaultSurfaceIntegrator
   
defaultSampler :: Sampler
defaultSampler = mkStratifiedSampler 2 2

pRenderer :: JobParser ()
pRenderer = pBlock $ do
   tName <- pString
   r <- case tName of

             "light" -> do
               ppp <- namedInt "passPhotons"
               return $ mkAnyRenderer $ mkLightTracer ppp
   
             "metropolis" -> do
                md <- namedInt "maxDepth"
                mpp <- namedFloat "mpp"
                nboot <- namedInt "bootstrap"
                plarge <- namedFloat "plarge"
                dspp <- namedInt "directSamples"
                return $ mkAnyRenderer $ mkMLT md mpp nboot plarge dspp
                
             "sppm" -> do
                n <- namedInt "photonCount"
                md <- namedInt "maxDepth"
                r <- namedFloat "radius"
                a <- option 0.8 $ namedFloat "alpha"
                return $ mkAnyRenderer $ mkSPPM n md r a
                
             "sampler" -> do
                sr <- pSamplerRenderer
                return $ mkAnyRenderer sr
                
             _ -> fail ("unknown renderer type " ++ tName)
             
   s <- getState
   setState s { renderer = r }
   
pSamplerRenderer :: JobParser SamplerRenderer
pSamplerRenderer = (flip namedBlock) "sampled" $ do
   s <- pSampler
   i <- pSurfaceIntegrator
   return $! mkSamplerRenderer s i
   
pSampler :: JobParser Sampler
pSampler = (flip namedBlock) "sampler" $ pString >>= \t -> case t of
   "stratified" -> do
      nx <- integ
      ny <- integ
      return $ mkStratifiedSampler nx ny
                
   "random" -> do
      ns <- integ
      return $ mkRandomSampler ns
                
   _ -> fail $ "unknown sampler type " ++ t
   
