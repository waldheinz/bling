
module SceneFile (parseScene) where

import Camera
import Math
import Primitive
import Scene
import Transform
import TriangleMesh

import Data.Array
import Debug.Trace
import Text.ParserCombinators.Parsec

data PState = PState {
   resX :: Int,
   resY :: Int,
   camera :: Camera,
   prims :: [AnyPrim]
   }

startState :: PState
startState = PState 1024 768 (pinHoleCamera (View (mkV(3, 7, -6)) (mkV(0,0,0)) (mkV(0, 1, 0)) 1.8 (4.0/3.0))) []

parseScene :: String -> Scene
parseScene s = either (error . show) (id) pr where
   pr = runParser sceneParser startState "unknown source"  s

type SceneParser a = GenParser Char PState a

sceneParser :: SceneParser Scene
sceneParser = do
   many line
   eof
   s <- getState
   return (mkScene [] (prims s) (camera s))
   
mesh :: SceneParser ()
mesh = do
   string "mesh"
   vertexCount <- try (do spaces; integ)
   faceCount <- try (do spaces; integ)
   char '\n'
   char 'm' -- the transform matrix
   m <- matrix
   char 'i' -- the inverse matrix
   i <- matrix
   vertices <- count vertexCount vertex
   let va = listArray (0, vertexCount-1) vertices
   faces <- count faceCount (face va)
   let mesh = mkMesh undefined (fromMatrix (m, i)) faces
   oldState <- getState 
   setState oldState {prims=[MkAnyPrim mesh] ++ prims oldState}
   
face :: (Array Int Vertex) -> SceneParser [Vertex]
face vs = do
   indices <- many1 (try (do (many (char ' ')); integ))
   char '\n'
   return (map (vs !) indices)
   
vertex :: SceneParser Vertex
vertex = do
   spaces
   x <- flt
   spaces
   y <- flt
   spaces
   z <- flt
   char '\n'
   return (Vertex (MkVector x y z))

matrix :: SceneParser [[Flt]]
matrix = do
   m <- count 4 (count 4 (try (do (many (char ' ')); flt)))
   char '\n'
   return m
   
line :: SceneParser ()
line = do try comment <|> try mesh <|> do try (char '\n'); return ()

-- | parse an integer
integ :: SceneParser Int
integ = do
   x <- many1 digit
   return (read x)

-- | parse a floating point number
flt :: SceneParser Flt
flt = do
  sign <- option 1 ( do s <- oneOf "+-"
                        return $ if s == '-' then (-1.0) else 1.0)
  i <- many digit
  d <- try (char '.' >> try (many digit))
  return $ sign * read (i++"."++d)

comment :: SceneParser ()
comment = do
   char '#'
   many (noneOf "\n")
   char '\n'
   return ()

   
