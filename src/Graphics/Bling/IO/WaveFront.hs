
module Graphics.Bling.IO.WaveFront (
   parseWaveFront
   ) where

import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore hiding (space)
import Graphics.Bling.Primitive.TriangleMesh

import qualified Data.ByteString.Lazy as BS
import Control.Monad (foldM)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Parsec.ByteString.Lazy ()

-- the state consists of a list of vertex positions and faces
data WFState s = WFState
   { stPoints     :: ! (MV.STVector s Point)
   , stPointCount :: ! Int
   , stFaces      :: ! (MV.STVector s Int)
   , stFaceCount  :: ! Int
   }
   
initialState :: ST s (WFState s)
initialState = do
   ps <- MV.new 64
   fs <- MV.new 64
   return $! WFState ps 0 fs 0

type WFParser s a = ParsecT BS.ByteString (WFState s) (ST s) a

-- | parses a WaveFront .obj file into a triangle mesh
parseWaveFront :: FilePath -> JobParser TriangleMesh
parseWaveFront fname = {-# SCC "parseWaveFront" #-} do
   inp <- readFileBS fname
   
   let
      res = runST $ do
         st <- initialState
         runPT waveFrontParser st fname inp
   
   case res of
        (Left e) -> fail $ show e
        (Right (ps, fs)) -> do
           s <- getState
           return $! mkTriangleMesh (transform s) (material s) ps fs Nothing Nothing

waveFrontParser :: WFParser s (V.Vector Point, V.Vector Int)
waveFrontParser = {-# SCC "waveFrontParser" #-} do
   skipMany $ pUV <|> vertex <|> face <|> ignore
   
   (WFState ps psc fs fsc) <- getState
   
   ps' <- lift $ V.freeze (MV.take psc ps)
   vs' <- lift $ V.freeze (MV.take fsc fs)

   return $! (V.force ps', V.force vs')

pUV :: WFParser s ()
pUV = {-# SCC "pUV" #-}do
   _ <- try $ string "vt"
   _ <- space >> float -- u
   _ <- space >> float -- v
   _ <- optional $ space >> float -- w
   return ()
   
ignore :: WFParser s ()
ignore = {-# SCC "ignore" #-} skipMany (noneOf "\n") >> eol

face :: WFParser s ()
face = {-# SCC "face" #-}do
   _ <- char 'f'
   
   indices <- many1 $ try $ do
      space
      vidx <- int
      _ <- option Nothing $ fmap Just $ char '/' >> int -- uv index
      _ <- option Nothing $ fmap Just $ char '/' >> int -- normal index
      return vidx

   optional space >> eol
   st <- getState
   
   let
      fs = stFaces st
      fsc = stFaceCount st
      add (v, l) e = addElement v l e >>= \v' -> return $! (v', l + 1)

   (fs', fsc') <- foldM add (fs, fsc) (triangulate [map pred indices])
   setState st { stFaces = fs', stFaceCount = fsc' }
   
vertex :: WFParser s ()
vertex = {-# SCC "vertex" #-}do
   _ <- char 'v'
   x <- space >> float
   y <- space >> float
   z <- space >> float
   _ <- optional $ space >> float -- ignore w component
   optional space >> eol
   
   st <- getState
   let cnt = stPointCount st
   ps' <- addElement (stPoints st) (stPointCount st) $ mkPoint (x, y, z)
   setState st { stPoints = ps', stPointCount = cnt + 1 }

space :: WFParser s ()
space = skipMany1 (char ' ') <?> "space"

eol :: WFParser s ()
eol = char '\n' >> return ()

-- | parse a floating point number
float :: (Monad m) => (ParsecT BS.ByteString u m) Flt
float = {-# SCC "float" #-} do
   sign <- option 1 $ do
      s <- oneOf "+-"
      return $! if s == '-' then (-1) else 1

   i <- many digit
   d <- option "0" (char '.' >> try (many digit))
   return $! sign * read (i ++ "." ++ d)

-- | parse an positive integer
int :: (Monad m) => (ParsecT BS.ByteString u m) Int
int = {-# SCC "int" #-} fmap read $ many1 digit

addElement :: MV.Unbox a => MV.STVector s a -> Int -> a -> WFParser s (MV.STVector s a)
addElement v cnt e = lift $ do
   let l = MV.length v
   
   v' <- if l < cnt + 1
            then MV.grow v l
            else return v

   MV.write v' cnt e
   return v'
