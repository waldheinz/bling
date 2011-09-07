
module Graphics.Bling.IO.LightParser ( pLight, pEmission ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Light
import Graphics.Bling.IO.ParserCore

--
-- parsing light sources
--

pLight :: JobParser ()
pLight = (flip namedBlock) "light" $ do
   t <- many1 alphaNum
   ws
   ls <- case t of
      "directional"  -> pDirectionalLight
      "point"        -> pPointLight
      "sunSky"       -> pSunSkyLight
      _              -> fail $ "unknown light type " ++ t

   s <- getState
   setState s { lights = ls ++ (lights s) }

pSunSkyLight :: JobParser [Light]
pSunSkyLight = do
   up <- namedVector "up"
   east <- ws >> namedVector "east"
   sunDir <- ws >> namedVector "sunDir"
   turb <- ws >> namedFloat "turbidity"
   return $ mkSunSkyLight up east sunDir turb

pPointLight :: JobParser [Light]
pPointLight = do
   r <- namedSpectrum "intensity"
   p <- ws >> namedVector "position"
   return $ [mkPointLight r p]

pDirectionalLight :: JobParser [Light]
pDirectionalLight = do
   s <- namedSpectrum "intensity"
   n <- ws >> namedVector "normal"
   return $ [mkDirectional s n]
   
pEmission :: JobParser ()
pEmission = (flip namedBlock) "emission" $ do
   spec <- do
      try (string "none" >> return Nothing)
      <|> (pSpectrum >>= (\s -> return (Just s)))
      
   s <- getState
   setState s { emit = spec }

