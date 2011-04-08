
module Graphics.Bling.IO.TransformParser (
   pTransform
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Math
import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore

--
-- parsing transformations
--

pTransform :: JobParser ()
pTransform = (namedBlock (many1 ts) "transform") >> return () where
   ts = choice [
      tIdentity, try tRotX,
      try tRotY,
      tRotZ,
      tScale,
      tTrans,
      tMatrix,
      ws]
      
tIdentity :: JobParser ()
tIdentity = do
   _ <- string "identity"
   s <- getState
   setState s { transform = identity }

tRotX :: JobParser ()
tRotX = do
   deg <- string "rotateX" >> ws >> flt
   s <- getState
   setState s { transform = concatTrans (transform s) (rotateX deg) }

tRotY :: JobParser ()
tRotY = do
   deg <- string "rotateY" >> ws >> flt
   s <- getState
   setState s { transform = concatTrans (transform s) (rotateY deg) }

tRotZ :: JobParser ()
tRotZ = do
   deg <- string "rotateZ" >> ws >> flt
   s <- getState
   setState s { transform = concatTrans (transform s) (rotateZ deg) }

tScale :: JobParser ()
tScale = do
   d <- string "scale" >> ws >> pVec
   s <- getState
   setState s { transform = concatTrans (transform s) (scale d) }

tTrans :: JobParser ()
tTrans = do
   d <- string "translate" >> ws >> pVec
   s <- getState
   setState s { transform = concatTrans (transform s) (translate d) }

tMatrix :: JobParser ()
tMatrix = do
   _ <- try (string "beginMatrix")
   m <- mtr 'm'
   i <- mtr 'i'
   _ <- ws >> string "endMatrix"
   let t = fromMatrix (m, i)
   s <- getState
   setState s { transform = concatTrans (transform s) t }

mtr :: Char -> JobParser [[Flt]]
mtr p = count 4 row where
   row = do
      _ <- ws >> char p
      r <- count 4 (try (do ws; flt))
      return r
   