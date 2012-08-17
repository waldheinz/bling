
module Graphics.Bling.IO.WaveFront (
   parseWaveFront
   ) where

import Graphics.Bling.Material
import Graphics.Bling.Reflection
import Graphics.Bling.IO.ParserCore hiding (space)
import Graphics.Bling.Primitive.TriangleMesh

import Debug.Trace

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lex.Lazy.Double as BSLD
import Data.Functor
import Control.Monad (foldM, forM, forM_)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- the state consists of a list of vertex positions and faces
data WFState s = WFState
   { stPoints     :: ! (MV.STVector s Point)
   , stPointCount :: ! Int
   , stFaces      :: ! (MV.STVector s Int)
   , stFaceCount  :: ! Int
   , stTexCoords  :: ! (MV.STVector s Float) -- the texture coorinates as found in the file
   , stTexCount   :: ! Int
   , stUVs        :: ! (MV.STVector s Float) -- the UVs as needed by TriangleMesh
   , stMtls       :: ! [(String, Int)] -- material name and first face where to apply it
   }
   
initialState :: ST s (WFState s)
initialState = do
   ps <- MV.new 64
   fs <- MV.new 64
   ts <- MV.new 64
   uvs <- MV.new 0
   return $! WFState ps 0 fs 0 ts 0 uvs []

type WFParser s a = ParsecT BS.ByteString (WFState s) (ST s) a

matIntervals :: Int -> [(String, Int)] -> [(String, Int, Int)]
matIntervals cnt mi = filt intervals where
   filt = filter (\(_, _, l) -> l > 0)
   starts = ("default", 0) : mi
   ends = map snd mi ++ [cnt]
   intervals = zipWith (\(n, s) e -> (n, s, e - s)) starts ends
   
-- | parses a WaveFront .obj file into a triangle mesh
parseWaveFront :: MaterialMap -> FilePath -> JobParser [TriangleMesh]
parseWaveFront mmap fname = {-# SCC "parseWaveFront" #-} do
   inp <- readFileBS fname
   
   case runST $ initialState >>= \st -> runPT waveFrontParser st fname inp of
      (Left e) -> fail $ show e
      (Right (ps, fs, uvs, mtls)) -> do
         st <- getState
         forM (matIntervals (V.length fs) (reverse mtls)) $ \(n, s, l) -> do
            let fs' = V.slice s l fs
            return $ mkTriangleMesh (transform st) (mmap n) ps fs' Nothing uvs
            
waveFrontParser :: WFParser s (V.Vector Point, V.Vector Int, Maybe (V.Vector Float), [(String, Int)])
waveFrontParser = {-# SCC "waveFrontParser" #-} do
   skipMany $ pUV <|> vertex <|> face <|> mtlspec <|> ignore
   
   (WFState ps psc fs fsc _ _ uvs mtls) <- getState
   ps' <- lift $ V.freeze (MV.take psc ps)
   vs' <- lift $ V.freeze (MV.take fsc fs)
   uvs' <- if MV.null uvs then return Nothing else do
      xx <- lift $ MV.grow uvs $ max 0 ((2 * (V.maximum vs')) - MV.length uvs)
      x <- lift $ V.freeze xx
      return $ Just x
      
   return $! (V.force ps', V.force vs', uvs', mtls)

mtlspec :: WFParser s ()
mtlspec = do
   _ <- try $ string "usemtl"
   n <- space >> (many $ noneOf "\n")
   s <- getState
   setState s { stMtls = ((n, stFaceCount s):stMtls s) }

pUV :: WFParser s ()
pUV = {-# SCC "pUV" #-} do
   _ <- try $ string "vt"
   u <- space >> float -- u
   v <- option 0 $ space >> float -- v
   _ <- optional $ space >> float -- w
   
   st <- getState
   
   let
      ts = stTexCoords st
      tsc = stTexCount st
      add (vv, l) e = addElement vv l e >>= \v' -> return $! (v', l + 1)

   (ts', tsc') <- foldM add (ts, tsc) [u, v]
   setState st { stTexCoords = ts', stTexCount = tsc' }
   
ignore :: WFParser s ()
ignore = {-# SCC "ignore" #-} skipMany (noneOf "\n") >> eol

face :: WFParser s ()
face = {-# SCC "face" #-}do
   _ <- char 'f'
   
   indices <- many1 $ try $ do
      space
      vidx <- int
      uvidx <- option Nothing $ fmap Just $ char '/' >> int -- uv index
      _ <- option Nothing $ fmap Just $ char '/' >> int -- normal index
      return (vidx, uvidx)
   
   optional space >> eol
      
   forM_ (triangulate [map (\(a, b) -> (pred a, b)) indices]) $ \(vidx, uvidx) -> do
      st <- getState
      let
         fs = stFaces st
         fsc = stFaceCount st
         coords = stTexCoords st
         uvs = stUVs st
         
      fs' <- addElement fs fsc vidx
      
      uvs'' <- case uvidx of
         Nothing -> return uvs
         (Just uvi) -> lift $ do
            sdf <- V.freeze coords
            u <- traceShow sdf $ MV.read coords $ uvi - 1
            v <- traceShow (uvi, vidx) $ MV.read coords $ uvi
            uvs' <- if MV.length uvs > (2 * vidx + 2)
                        then return uvs
                        else MV.grow uvs $ (2 * vidx + 2) - MV.length uvs
            MV.write uvs' (2 * vidx) u
            MV.write uvs' (2 * vidx + 1) v
            return uvs'
      
      setState st { stFaces = fs', stFaceCount = fsc+1, stUVs = uvs'' }
   
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
float :: (Monad m) => (ParsecT BS.ByteString u m) Float
float = {-# SCC "float" #-} do
   s <- getInput
   case BSLD.readDouble s of
      Just (v, s') -> (realToFrac v) <$ setInput s'
      Nothing -> fail "error parsing float"
      
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
