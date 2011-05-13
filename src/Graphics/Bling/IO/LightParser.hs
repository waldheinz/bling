
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
   lf <- case t of
      "directional"  -> pDirectionalLight
      "point"        -> pPointLight
      "sunSky"       -> pSunSkyLight
      _              -> fail $ "unknown light type " ++ t
      
   s <- getState
   let l = lf (currId s)
   setState s { lights = l : (lights s), currId = (currId s) + 1 }

type LightFactory = Int -> Light

pSunSkyLight :: JobParser LightFactory
pSunSkyLight = do
   up <- namedVector "up"
   east <- ws >> namedVector "east"
   sunDir <- ws >> namedVector "sunDir"
   turb <- ws >> namedFloat "turbidity"
   return $ mkSunSky up east sunDir turb

pPointLight :: JobParser LightFactory
pPointLight = do
   r <- namedSpectrum "intensity"
   p <- ws >> namedVector "position"
   return $ mkPointLight r p

pDirectionalLight :: JobParser LightFactory
pDirectionalLight = do
   s <- namedSpectrum "intensity"
   n <- ws >> namedVector "normal"
   return $ mkDirectional s n
   
pEmission :: JobParser ()
pEmission = (flip namedBlock) "emission" $ do
   spec <- do
      try (string "none" >> return Nothing)
      <|> (pSpectrum >>= (\s -> return (Just s)))
      
   s <- getState
   setState s { emit = spec }

