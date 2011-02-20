
module WaveFront where

import Primitive
import TriangleMesh

import Debug.Trace
import Data.Vector.Mutable hiding (read)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token(float)

data WFState s = WFState {
   vertices :: MVector s Vertex
   }

type WFParser s a = GenParser Char (WFState s) a
   
waveFront :: Parser Primitive
waveFront = do
   many line
   eof
   return $ Group []

line =
   (do try vertex; return () )
   <|> try face
   <|> ignore
   
ignore :: Parser ()
ignore = do
   ignored <- many (noneOf "\n")
   eol
   trace ignored $ return ()
   
face :: Parser ()
face = do
   char 'f'
   indices <- many1 (try (do spaces; integ))
   eol
   return ()
   
vertex :: Parser Vertex
vertex = do
   char 'v'
   spaces
   x <- flt
   spaces
   y <- flt
   spaces
   z <- flt
   eol
   return $ Vertex (x, y, z)
   
eol = char '\n'

integ :: Parser Int
integ = do
   x <- many1 digit
   return $ read x

flt :: Parser Float
flt = do
  sign <- option 1 ( do s <- oneOf "+-"
                        return $ if s == '-' then (-1.0) else 1.0)
  i <- many digit
  d <- try (char '.' >> try (many digit))
  return $ sign * read (i++"."++d)
