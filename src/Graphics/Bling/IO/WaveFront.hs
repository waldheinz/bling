
module Graphics.Bling.IO.WaveFront (
   parseWaveFront
   ) where

import Graphics.Bling.Material
import Graphics.Bling.Reflection
import Graphics.Bling.IO.ParserCore hiding (space)
import Graphics.Bling.Primitive.TriangleMesh

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lex.Lazy.Double as BSLD
import Data.Functor
import Data.STRef
import Control.Monad (forM, forM_, when)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data GrowVec s a = GV ! (STRef s (MV.STVector s a)) ! (STRef s Int)

addElement :: (MV.Unbox a) => GrowVec s a -> a -> ST s ()
addElement (GV vr cntr) e = do
   v <- readSTRef vr
   cnt <- readSTRef cntr
   
   let l = MV.length v
   when (l < (cnt + 1)) $ MV.grow v l >>= writeSTRef vr
   
   v' <- readSTRef vr
   MV.write v' cnt e
   modifySTRef cntr (+1)
   
newGV :: (MV.Unbox a) => ST s (GrowVec s a)
newGV = do
   v <- MV.new 64
   vr <- newSTRef v
   cr <- newSTRef 0
   return $! GV vr cr

gvLength :: GrowVec s a -> ST s Int
gvLength (GV _ cr) = readSTRef cr

gvFreeze :: (MV.Unbox a) => GrowVec s a -> ST s (V.Vector a)
gvFreeze (GV vr cr) = do
   v <- readSTRef vr
   c <- readSTRef cr
   V.freeze (MV.take c v)

-- the state consists of a list of vertex positions and faces
data WFState s = WFState
   { stPoints     :: ! (GrowVec s Point)
   , stFaces      :: ! (GrowVec s (Int, Int)) -- (point index, uv index)
   , stTexCoords  :: ! (GrowVec s Float) -- the UVs as found in the file
   , stMtls       :: ! [(String, Int)] -- material name and first face where to apply it
   }
   
initialState :: ST s (WFState s)
initialState = do
   ps <- newGV
   fs <- newGV
   ts <- newGV
   return $! WFState ps fs ts []

type WFParser s a = ParsecT BS.ByteString (WFState s) (ST s) a

matIntervals :: Int -> [(String, Int)] -> [(String, Int, Int)]
matIntervals cnt mi = filt intervals where
   filt = filter (\(_, _, l) -> l > 0)
   starts = ("default", 0) : mi
   ends = map snd mi ++ [cnt]
   intervals = zipWith (\(n, s) e -> (n, s, e - s)) starts ends
   
-- | parses a WaveFront .obj file into triangle meshes
parseWaveFront :: MaterialMap -> FilePath -> JobParser [TriangleMesh]
parseWaveFront mmap fname = {-# SCC "parseWaveFront" #-} do
   inp <- readFileBS fname
   
   case runST $ initialState >>= \st -> runPT waveFrontParser st fname inp of
      (Left e) -> fail $ show e
      (Right (ps, fs, uvs, mtls)) -> do
         let
            (pis, uvis) = V.unzip fs -- (point indices, uv indices)
  --          muv = V.generate (2 * V.length pis) $ \i ->
  --             let (i', o) = divMod i 2 in uvs V.! (2 * (uvis V.! i') + o)
            
         st <- getState
         forM (matIntervals (V.length pis) (reverse mtls)) $ \(n, s, l) -> do
            let
               fs' = V.slice s l pis
               muv = V.generate (2 * l) $ \i ->
                  let (i', o) = divMod i 2 in uvs V.! (2 * (uvis V.! (i' + s)) + o)
                  
            return $! mkTriangleMesh (transform st) (mmap n) ps fs' Nothing (Just muv)
            
waveFrontParser :: WFParser s (V.Vector Point, V.Vector (Int, Int), V.Vector Float, [(String, Int)])
waveFrontParser = {-# SCC "waveFrontParser" #-} do
   skipMany $ pUV <|> vertex <|> face <|> mtlspec <|> ignore
   
   (WFState ps fs uvs mtls) <- getState
   
   ps' <- lift $ gvFreeze ps
   vs' <- lift $ gvFreeze fs
   uvs' <- lift $ gvFreeze uvs
   
   return $! (ps', vs', uvs', mtls)

mtlspec :: WFParser s ()
mtlspec = do
   _ <- try $ string "usemtl"
   n <- space >> (many $ noneOf "\n")
   s <- getState
   nf <- lift $ gvLength (stFaces s)
   setState s { stMtls = ((n, nf):stMtls s) }

pUV :: WFParser s ()
pUV = do
   _ <- try $ string "vt"
   u <- space >> float -- u
   v <- option 1 $ space >> float -- v
   _ <- optional $ space >> float -- w
   
   st <- getState
   lift $ mapM_ (addElement $ stTexCoords st) [u, v]
   
ignore :: WFParser s ()
ignore = skipMany (noneOf "\n") >> eol

face :: WFParser s ()
face = do
   _ <- char 'f'
   
   indices <- many1 $ try $ do
      space
      vidx <- int
      uvidx <- option 0 $ char '/' >> int -- uv index
      _ <- option Nothing $ fmap Just $ char '/' >> int -- normal index
      return (vidx, uvidx)
   
   optional space >> eol
      
   forM_ (triangulate [map (\(a, b) -> (pred a, pred b)) indices]) $ \f -> do
      st <- getState
      let
         fs = stFaces st
         
      lift $ addElement fs f
--      setState st { stFaces = fs', stFaceCount = fsc+1 }
   
vertex :: WFParser s ()
vertex = do
   _ <- char 'v'
   x <- space >> float
   y <- space >> float
   z <- space >> float
   _ <- optional $ space >> float -- ignore w component
   optional space >> eol
   
   st <- getState
   lift (addElement (stPoints st) $ mkPoint (x, y, z))

space :: WFParser s ()
space = skipMany1 (char ' ') <?> "space"

eol :: WFParser s ()
eol = char '\n' >> return ()

-- | parse a floating point number
float :: (Monad m) => (ParsecT BS.ByteString u m) Float
float = do
   s <- getInput
   case BSLD.readDouble s of
      Just (v, s') -> (realToFrac v) <$ setInput s'
      Nothing -> fail "error parsing float"
      
-- | parse an positive integer
int :: (Monad m) => (ParsecT BS.ByteString u m) Int
int = do
   s <- getInput
   case BSC.readInt s of
      Just (v, s') -> v <$ setInput s'
      Nothing -> fail "error parsing integer"


