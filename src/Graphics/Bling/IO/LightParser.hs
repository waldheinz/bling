
module Graphics.Bling.IO.LightParser ( pLight, pEmission ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Light
import Graphics.Bling.IO.ParserCore

--
-- parsing light sources
--

pLight :: JobParser ()
pLight = do
   l <- pDirectionalLight
   s <- getState
   setState s { lights = l : (lights s) }

pDirectionalLight :: JobParser Light
pDirectionalLight = do
   try (string "beginDirectionalLight") >> ws
   s <- pSpectrum  <|> fail "missing spectrum"
   _ <- ws >> (string "normal" <|> fail "missing normal")
   n <- ws >> (pVec <|> fail "could not parse normal")
   _ <- ws >> string "endDirectionalLight"
   return (mkDirectional s n)
   
pEmission :: JobParser ()
pEmission = (flip namedBlock) "emission" $ do
   spec <- do
      try (string "none" >> return Nothing)
      <|> (pSpectrum >>= (\s -> return (Just s)))
      
   s <- getState
   setState s { emit = spec }

