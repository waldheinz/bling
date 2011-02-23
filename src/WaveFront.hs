
module WaveFront (
   waveFrontParser
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

waveFrontParser :: WFParser Primitive
waveFrontParser = do
   many line
   (WFState _ tris) <- getState
   eof
   return $ Group (map (\ t-> mkPrim' t (measuredMaterial BluePaint) Nothing) tris)

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
   let tvs = map (verts !!) indices
   setState (WFState verts (triangulate tvs ++ tris))
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
