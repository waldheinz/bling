
module WaveFront (
      waveFrontParser,
      parseWaveFront
   ) where

import Lafortune
import Math
import Primitive
import TriangleMesh

import Control.Monad.ST
import Debug.Trace
import Data.Vector.Mutable hiding (read)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token(float)

data WFState = WFState {
   vertices :: [Vertex],
   tris :: [Triangle]
   }

type WFParser a = GenParser Char WFState a

parseWaveFront :: String -> Primitive
parseWaveFront s = either (error . show) (id) pr where
   pr = runParser waveFrontParser (WFState [] []) "unknown"  s

waveFrontParser :: WFParser Primitive
waveFrontParser = do
   many line
   (WFState _ tris) <- getState
   eof
   return $ Group (map (\ t-> MkAnyPrim $ mkPrim t (measuredMaterial BluePaint) Nothing) tris)

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
   char 'f'
   indices <- many1 (try (do spaces; integ))
   eol
   (WFState verts tris) <- getState
   let tvs = map (verts !!) $ map pred indices
   setState (WFState verts (triangulate (reverse tvs) ++ tris))
   return ()
   
vertex :: WFParser ()
vertex = do
   char 'v'
   spaces
   x <- flt
   spaces
   y <- flt
   spaces
   z <- flt
   eol
   (WFState verts tris) <- getState
   setState (WFState (verts ++ [Vertex (MkVector x y z)]) tris)
   return ()
   
eol = char '\n'

integ :: WFParser Int
integ = do
   x <- many1 digit
   return $ read x

flt :: WFParser Float
flt = do
  sign <- option 1 ( do s <- oneOf "+-"
                        return $ if s == '-' then (-1.0) else 1.0)
  i <- many digit
  d <- try (char '.' >> try (many digit))
  return $ sign * read (i++"."++d)
