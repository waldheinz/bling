
module Graphics.Bling.IO.WaveFront (
      parseWaveFront
   ) where

import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore
import Graphics.Bling.Primitive.TriangleMesh

import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Text.Parsec.String

type Triangle = [Int]

data WFState = WFState [Point] [Triangle]

type WFParser a = GenParser Char WFState a

parseWaveFront :: FilePath -> JobParser TriangleMesh
parseWaveFront fname = do
   inp <- readFileString fname
   let res = runParser waveFrontParser (WFState [] []) fname inp
   case res of
        (Left e) -> fail $ show e
        (Right (WFState ps vs)) -> do
           s <- getState
           let ps' = V.fromList $ reverse ps
           let vs' = V.fromList $ concatMap (take 3) vs
           return $ mkTriangleMesh (transform s) (material s) ps' vs' Nothing Nothing

waveFrontParser :: WFParser WFState
waveFrontParser = do
   _ <- many line
   getState

line :: WFParser ()
line =
   (do try vertex; return () )
   <|> try face
   <|> ignore

ignore :: WFParser ()
ignore = do
   ignored <- many (noneOf "\n")
   eol
   trace ignored $ return ()

face :: WFParser ()
face = do
   _ <- char 'f'
   indices <- many1 (try (do spaces; integ))
   eol
   (WFState vs ts) <- getState
   setState (WFState vs ((map pred indices) : ts))
   
vertex :: WFParser ()
vertex = do
   _ <- char 'v'
   spaces
   x <- flt
   spaces
   y <- flt
   spaces
   z <- flt
   eol
   (WFState vs ts) <- getState
   setState (WFState (mkPoint (x, y, z) : vs) ts)

eol :: WFParser ()
eol = char '\n' >> return ()
