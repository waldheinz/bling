
module Graphics.Bling.IO.WaveFront (
   parseWaveFront
   ) where

import Graphics.Bling.Transform
import Graphics.Bling.IO.ParserCore hiding (space)
import Graphics.Bling.Primitive.TriangleMesh

import qualified Data.Vector.Unboxed as V
import Text.Parsec.String

-- a face is a list of vertex indices
type Face = [Int]

-- the state consists of a list of vertex positions and faces
data WFState = WFState [Point] [Face]

type WFParser a = GenParser Char WFState a

-- | parses a WaveFront .obj file into a triangle mesh
parseWaveFront :: FilePath -> JobParser TriangleMesh
parseWaveFront fname = {-# SCC "parseWaveFront" #-} do
   inp <- readFileString fname
   let res = runParser waveFrontParser (WFState [] []) fname inp
   case res of
        (Left e) -> fail $ show e
        (Right (ps, fs)) -> do
           s <- getState
           return $! mkTriangleMesh (transform s) (material s) ps fs Nothing Nothing

waveFrontParser :: WFParser (V.Vector Point, V.Vector Int)
waveFrontParser = {-# SCC "waveFrontParser" #-}do
   _ <- many $ pUV <|> vertex <|> face <|> ignore
   
   (WFState ps fs) <- getState
   
   let ps' = V.fromList $ reverse ps
   let vs' = V.fromList $ triangulate fs

   return (ps', vs')

pUV :: WFParser ()
pUV = {-# SCC "pUV" #-}do
   _ <- try $ string "vt"
   _ <- space >> flt' -- u
   _ <- space >> flt' -- v
   _ <- optional $ space >> flt' -- w
   return ()
   
ignore :: WFParser ()
ignore = {-# SCC "ignore" #-}do
   _ <- many (noneOf "\n")
   eol
   return ()

face :: WFParser ()
face = {-# SCC "face" #-}do
   _ <- char 'f'
   
   indices <- many1 $ try $ do
      space
      vidx <- integ'
      _ <- option Nothing $ char '/' >> integ' >>= return . Just -- uv index
      _ <- option Nothing $ char '/' >> integ' >>= return . Just -- normal index
      return vidx

   optional space >> eol
   (WFState vs ts) <- getState
   setState (WFState vs (map pred indices : ts))
   
vertex :: WFParser ()
vertex = {-# SCC "vertex" #-}do
   _ <- char 'v'
   x <- space >> flt'
   y <- space >> flt'
   z <- space >> flt'
   _ <- optional $ space >> flt' -- ignore w component
   optional space >> eol
   (WFState vs ts) <- getState
   setState (WFState (mkPoint (x, y, z) : vs) ts)

space :: WFParser ()
space = (many1 (char ' ') <?> "space") >> return ()

eol :: WFParser ()
eol = char '\n' >> return ()
