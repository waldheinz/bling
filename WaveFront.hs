
module WaveFront where

import Primitive
import TriangleMesh

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token(float)

waveFront :: Parser Primitive
waveFront = do return $ Group []

tidy :: Parser [String]
tidy = many (try content)

content :: Parser String
content = do
   x <- noneOf "#"
   xs <- manyTill (noneOf "\n") eol
   return $ x:xs
   
comment :: Parser String
comment = do
   char '#'
   x <- manyTill (noneOf "\n") eol
   return x

vertices :: Parser [Vertex]
vertices = many1 vertex

vertex :: Parser Vertex
vertex = do
   char 'v'
   spaces
   x <- number
   spaces
   y <- number
   spaces
   z <- number
   eol
   return $ Vertex (x, y, z)
   
eol = char '\n'

number :: Parser Float
number = do
  sign <- option 1 ( do s <- oneOf "+-"
                        return $ if s == '-' then (-1.0) else (1.0))
  i <- many digit
  d <- try (char '.' >> try (many (digit)))
  return $ sign*(read (i++"."++d))
