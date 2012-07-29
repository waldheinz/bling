
module Graphics.Bling.IO.TransformParser (
   pTransform, pGlobalTrans
   ) where

import Data.Monoid

import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore

--
-- parsing transformations
--

pGlobalTrans :: JobParser ()
pGlobalTrans = do
   t <- pTransform
   s <- getState
   setState s { transform = t <> (transform s) }
   
pTransform :: JobParser Transform
pTransform = pBlock $ do
   ts <- many anyTransform
   return $! mconcat ts

anyTransform :: JobParser Transform
anyTransform = choice [
   try tRotX, try tRotY, tRotZ,
   tScale, tTrans, tMatrix, tLookAt]

tLookAt :: JobParser Transform
tLookAt = (flip namedBlock) "lookAt" $ do
   pos <- namedVector "pos" <?> "position"
   look <- namedVector "look" <?> "look at point"
   up <- namedVector "up" <?> "up vector"
   return $! lookAt pos look up
   
tRotX :: JobParser Transform
tRotX = string "rotateX" >> ws >> flt >>= return . rotateX

tRotY :: JobParser Transform
tRotY = string "rotateY" >> ws >> flt >>= return . rotateY

tRotZ :: JobParser Transform
tRotZ = string "rotateZ" >> ws >> flt >>= return . rotateZ

tScale :: JobParser Transform
tScale = string "scale" >> ws >> pVec >>= return . scale

tTrans :: JobParser Transform
tTrans = string "translate" >> ws >> pVec >>= return . translate

tMatrix :: JobParser Transform
tMatrix = (flip namedBlock) "matrix" $ mtr 'm' >>= return . fromMatrix'

mtr :: Char -> JobParser [[Float]]
mtr p = count 4 row where
   row = do
      _ <- char p
      r <- count 4 (try (do ws; flt))
      _ <- ws
      return r
