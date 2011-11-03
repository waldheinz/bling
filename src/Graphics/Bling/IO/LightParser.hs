
module Graphics.Bling.IO.LightParser (
   
   pLight, pEmission
   
   ) where

import Graphics.Bling.Light
import Graphics.Bling.IO.MaterialParser
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.IO.TransformParser

--
-- parsing light sources
--

pLight :: JobParser ()
pLight = pBlock $ do
   t <- pString
   ls <- case t of
      "directional"  -> pDirectionalLight
      "infinite"     -> pInfiniteArea
      "point"        -> pPointLight
      _              -> fail $ "unknown light type " ++ t

   s <- getState
   setState s { lights = ls : (lights s) }

pInfiniteArea :: JobParser Light
pInfiniteArea = do
   t <- pTransform
   l <- namedSpectrumMap "l"
   return $ mkInfiniteAreaLight l t
   
pPointLight :: JobParser Light
pPointLight = do
   r <- namedSpectrum "intensity"
   p <- namedVector "position"
   return $ mkPointLight r p

pDirectionalLight :: JobParser Light
pDirectionalLight = do
   s <- namedSpectrum "intensity"
   n <- namedVector "normal"
   return $ mkDirectional s n
   
pEmission :: JobParser ()
pEmission = pBlock $ do
   spec <- do
      try (string "none" >> return Nothing)
      <|> (pSpectrum >>= (\s -> return (Just s)))
      
   s <- getState
   setState s { emit = spec }

