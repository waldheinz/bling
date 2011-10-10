
module Graphics.Bling.IO.WaveFront (
      parseWaveFront
   ) where

import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore hiding (space)
import Graphics.Bling.Primitive.TriangleMesh

import Data.List (tails)
import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Text.Parsec.String

type Face = [Int]

data WFState = WFState [Point] [Face]

type WFParser a = GenParser Char WFState a

parseWaveFront :: FilePath -> JobParser TriangleMesh
parseWaveFront fname = do
   inp <- readFileString fname
   let res = runParser waveFrontParser (WFState [] []) fname inp
   case res of
        (Left e) -> fail $ show e
        (Right (ps, fs)) -> do
           s <- getState
           return $ mkTriangleMesh (transform s) (material s) ps fs Nothing Nothing

waveFrontParser :: WFParser (V.Vector Point, V.Vector Int)
waveFrontParser = do
   _ <- many line
   
   (WFState ps fs) <- getState
   
   let ps' = V.fromList $ reverse ps
   let vs' = V.fromList $ triangulate fs

   return (ps', vs')

triangulate :: [Face] -> [Int]
triangulate = concatMap go where
   go [] = []
   go (f0:fs) = concatMap (f0:) $
                        map (take 2) $
                        takeWhile (\x -> length x >= 2) $
                        tails fs

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
   
   indices <- many1 $ try $ do
      space
      vidx <- integ
      _ <- option Nothing $ char '/' >> integ >>= \t -> return $ Just t -- uv index
      _ <- option Nothing $ char '/' >> integ >>= \t -> return $ Just t -- normal index
      return vidx

   optional space >> eol
   (WFState vs ts) <- getState
   setState (WFState vs (map pred indices : ts))
   
vertex :: WFParser ()
vertex = do
   _ <- char 'v'
   x <- space >> flt
   y <- space >> flt
   z <- space >> flt
   eol
   (WFState vs ts) <- getState
   setState (WFState (mkPoint (x, y, z) : vs) ts)

space :: WFParser ()
space = (many1 (char ' ') <?> "space") >> return ()

eol :: WFParser ()
eol = char '\n' >> return ()
