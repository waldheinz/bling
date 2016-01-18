
{-# LANGUAGE BangPatterns #-}

module Graphics.Bling.IO.WaveFront (
   parseWaveFront
   ) where

import Graphics.Bling.Material
import Graphics.Bling.Reflection
import Graphics.Bling.IO.ParserCore hiding (space)
import Graphics.Bling.Primitive
import Graphics.Bling.Primitive.TriangleMesh
import Graphics.Bling.Utils

import qualified Data.ByteString.Lazy as BS
--import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lex.Integral as BSLI
import qualified Data.ByteString.Lex.Fractional as BSLF
import Data.Functor
import Control.Monad (forM, forM_, liftM)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type STUGrowVec s a = GrowVec MV.MVector s a

data WFState s = WFState
   { stPoints     :: ! (STUGrowVec s Point)           -- vertex positions
   , stNormals    :: ! (STUGrowVec s Normal)          -- vertex normals
   , stTexCoords  :: ! (STUGrowVec s (Float, Float))  -- the UVs as found in the file
   , stFaces      :: ! (STUGrowVec s (Int, Int, Int)) -- (point index, uv index, normal index)
   , stMtls       :: ! [(String, Int)]                -- material name and first face where to apply it
   }

data WFData = WFData
   { wfPoints     :: ! (V.Vector Point)
   , wfNormals    :: ! (V.Vector Normal)
   , wfTexCoords  :: ! (V.Vector (Float, Float))
   , wfFaces      :: ! (V.Vector (Int, Int, Int))
   }

wfTriVerts :: WFData -> Int -> TriVerts
wfTriVerts d i = (p1, p2, p3) where
   p1 = (wfPoints d) V.! (let (i', _, _) = (wfFaces d) V.! i in i')
   p2 = (wfPoints d) V.! (let (i', _, _) = (wfFaces d) V.! (i+1) in i')
   p3 = (wfPoints d) V.! (let (i', _, _) = (wfFaces d) V.! (i+2) in i')

wfTriUVs :: WFData -> Int -> TriUVs
wfTriUVs d i
   | i1 >= 0 && i2 >= 0 && i3 >= 0 = (u1, v1, u2, v2, u3, v3)
   | otherwise = triangleDefaultUVs
   where
      (u1, v1) = (wfTexCoords d) V.! i1
      (u2, v2) = (wfTexCoords d) V.! i2
      (u3, v3) = (wfTexCoords d) V.! i3

      i1 = let (_, i', _) = (wfFaces d) V.! (i+0) in i'
      i2 = let (_, i', _) = (wfFaces d) V.! (i+1) in i'
      i3 = let (_, i', _) = (wfFaces d) V.! (i+1) in i'

wfTriNormals :: WFData -> Int -> Maybe TriNorms
wfTriNormals d i
   | i1 < 0 && i2 < 0 && i3 < 0 = Nothing
   | otherwise = Just (n1, n2, n3)
   where
      n1 = (wfNormals d) V.! i1
      n2 = (wfNormals d) V.! i2
      n3 = (wfNormals d) V.! i3

      i1 = let (_, _, i') = (wfFaces d) V.! (i+0) in i'
      i2 = let (_, _, i') = (wfFaces d) V.! (i+1) in i'
      i3 = let (_, _, i') = (wfFaces d) V.! (i+1) in i'

mkWFTri :: Material -> WFData -> Int -> Primitive
mkWFTri !mat !d !i = prim where
   prim = Primitive tint ints bounds Nothing shade
   tint = triangleIntersect mat prim (wfTriVerts d i) (wfTriUVs d i)
   ints = triangleIntersects (wfTriVerts d i)
   bounds = triangleBounds (wfTriVerts d i)
   shade = maybe const triangleShadingGeometry (wfTriNormals d i)

initialState :: ST s (WFState s)
initialState = do
   ps <- gvNew
   fs <- gvNew
   ts <- gvNew
   ns <- gvNew
   return $! WFState ps ns fs ts []

type WFParser s a = ParsecT BS.ByteString (WFState s) (ST s) a

matIntervals :: Int -> [(String, Int)] -> [(String, Int, Int)]
matIntervals cnt mi = filter (\(_, _, l) -> l > 0) intervals where
   starts = ("default", 0) : mi
   ends = map snd mi ++ [cnt]
   intervals = zipWith (\(n, s) e -> (n, s, e - s)) starts ends

-- | parses a WaveFront .obj file into triangle meshes
parseWaveFront :: MaterialMap -> FilePath -> JobParser [Primitive]
parseWaveFront mmap fname = {-# SCC "parseWaveFront" #-} do
   inp <- readFileBS fname

   trans <- transform <$> getState

   case runST $ initialState >>= \st -> runPT waveFrontParser st fname inp of
      (Left e) -> fail $ show e
      (Right (ps, ns, fs, uvs, mtls)) -> do
         let
            pst = V.map (transPoint trans) ps
            wfd = WFData pst ns uvs fs
            matInts = matIntervals (V.length fs `quot` 3) (reverse mtls)

         liftM concat $ forM matInts $ \(n, s, l) -> do
            forM [s, s+3 .. (s + l - 1)] $ \i -> do
               return $! mkWFTri (mmap n) wfd i

waveFrontParser :: WFParser s (V.Vector Point, V.Vector Normal, V.Vector (Int, Int, Int), V.Vector (Float, Float), [(String, Int)])
waveFrontParser = {-# SCC "waveFrontParser" #-} do
   skipMany $ pNormal <|> pUV <|> vertex <|> face <|> mtlspec <|> ignore

   (WFState ps ns fs uvs mtls) <- getState

   ps' <- lift $ gvFreeze ps
   ns' <- lift $ gvFreeze ns
   vs' <- lift $ gvFreeze fs
   uvs' <- lift $ gvFreeze uvs

   return $! (ps', ns', uvs', vs', mtls)

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
   lift $ gvAdd (stTexCoords st) (u, v)

ignore :: WFParser s ()
ignore = skipMany (noneOf "\n") >> eol

face :: WFParser s ()
face = do
   _ <- char 'f'

   indices <- many1 $ try $ do
      space
      vidx <- int
      uvidx <- option 0 $ char '/' >> option 0 int -- uv index
      nidx <- option 0 $ char '/' >> int -- normal index
      return (vidx, uvidx, nidx)

   optional space >> eol
   faces <- stFaces <$> getState
   forM_ (triangulate [map (\(a, b, c) -> (pred a, pred b, pred c)) indices]) $ \f ->
      lift $ gvAdd faces f

vertex :: WFParser s ()
vertex = do
   _ <- char 'v'
   x <- space >> float
   y <- space >> float
   z <- space >> float
   _ <- optional $ space >> float -- ignore w component
   optional space >> eol

   st <- getState
   lift (gvAdd (stPoints st) $ mkPoint (x, y, z))

pNormal :: WFParser s ()
pNormal = do
   _ <- try $ string "vn"
   x <- space >> float
   y <- space >> float
   z <- space >> float
   optional space >> eol

   st <- getState
   lift (gvAdd (stNormals st) $ normalize (mkPoint (x, y, z)))

space :: WFParser s ()
space = skipMany1 (char ' ') <?> "space"

eol :: WFParser s ()
eol = void $ char '\n'

-- | parse a floating point number
float :: (Monad m) => (ParsecT BS.ByteString u m) Float
float = do
   s <- getInput
   case BSLF.readSigned BSLF.readDecimal $ BS.toStrict s of
      Just (v, s') -> setInput (BS.fromStrict s') >> pure v
      Nothing -> fail "error parsing float"

-- | parse an positive integer
int :: (Monad m) => (ParsecT BS.ByteString u m) Int
int = do
   s <- getInput
   case BSLI.readDecimal (BS.toStrict s) of
      Just (v, s') -> v <$ setInput (BS.fromStrict s')
      Nothing -> fail "error parsing integer"
