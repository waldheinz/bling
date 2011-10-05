
module Graphics.Bling.IO.TransformParser (
   pTransform
   ) where

import Graphics.Bling.Math
import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore

--
-- parsing transformations
--

pTransform :: JobParser ()
pTransform = pBlock (many1 ts) >> return () where
   ts = choice [ ws,
      tIdentity, try tRotX,
      try tRotY,
      tRotZ,
      tScale,
      tTrans,
      tMatrix,
      tLookAt ]

tLookAt :: JobParser ()
tLookAt = (flip namedBlock) "lookAt" $ do
   pos <- namedVector "pos"
   look <- ws >> namedVector "look"
   up <- ws >> namedVector "up"
   s <- getState
   setState s { transform = lookAt pos look up }
   
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
tMatrix = (flip namedBlock) "matrix" $ do
   m <- mtr 'm'
   let t = fromMatrix' m
   s <- getState
   setState s { transform = concatTrans (transform s) t }

mtr :: Char -> JobParser [[Flt]]
mtr p = count 4 row where
   row = do
      _ <- char p
      r <- count 4 (try (do ws; flt))
      _ <- ws
      return r
   
