
module Graphics.Bling.IO.ParserCore (
   module PS,
      
   -- * Data Types

   JobParser, PState(..), nextId,
   
   -- * Core Parsing Primitives
   
   flt, ws, pVec, pSpectrum, pBlock, namedBlock, namedInt, namedFloat,
   namedVector, namedSpectrum, integ, pString, pQString,

   -- * Reading External Resources

   readFileBS, readFileString
   
   ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS
import System.FilePath as FP
import Text.Parsec.Prim as PS
import Text.Parsec.Combinator as PS
import Text.Parsec.Char as PS
import Text.Parsec.String()

import Graphics.Bling.Camera
import Graphics.Bling.Filter
import Graphics.Bling.Light
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Rendering
import Graphics.Bling.Reflection
import Graphics.Bling.Spectrum

type JobParser a = ParsecT String PState IO a

data PState = PState {
   resX :: Int,
   resY :: Int,
   renderer :: AnyRenderer,
   pxFilter :: Filter, -- ^ the pixel filtering function
   camera :: Camera,
   transform :: Transform,
   material :: Material,
   emit :: Maybe Spectrum, -- ^ the emission for the next primitives
   lights :: [Light],
   prims :: [AnyPrim],
   currId :: Int,
   basePath :: FilePath
   }

nextId :: JobParser Int
nextId = do
   s <- getState
   let nid = currId s
   setState s { currId = nid + 1 }
   return nid

pString :: JobParser String
pString = many1 alphaNum

-- | parses a "quoted" string, and returns it without the quotes
pQString :: JobParser String
pQString = do
   dquote
   r <- many qcont
   dquote
   return $ concat r
   <?> "quoted string"
   where
      qtext = noneOf "\\\"\r\n"
      qcont = many1 qtext <|> quoted_pair
      dquote = (do _ <- char '"'; return ()) <?> "double quote"
      quoted_pair = do _ <- char '\\'
                       r <- noneOf "\r\n"
                       return ['\\',r]
                    <?> "quoted pair"
                    
comment :: JobParser ()
comment = char '#' >> many (noneOf "\n") >> char '\n' >> return () <?> "comment"

-- | skips over whitespace and comments
ws :: JobParser ()
ws = many1 (choice [space >> return (), comment]) >> return ()

-- | parse a floating point number
flt :: (Monad m) => (ParsecT String u m) Flt
{-# INLINE flt #-}
flt = do
  sign <- option 1 ( do s <- oneOf "+-"
                        return $ if s == '-' then (-1.0) else 1.0)
  i <- many digit
  d <- option "0" (char '.' >> try (many digit))
  return $ sign * read (i++"."++d)

-- | parse a vector
pVec :: JobParser Vector
pVec = do
   x <- flt
   y <- ws >> flt
   z <- ws >> flt
   return (Vector x y z)

namedVector :: String -> JobParser Vector
namedVector n = do string n >> ws; pVec

pSpectrum :: JobParser Spectrum
pSpectrum = do
   t <- many alphaNum
   ws
   case t of
      "rgb" -> do
         r <- flt
         g <- ws >> flt
         b <- ws >> flt
         return (fromRGB (r, g, b))
         
      "spd" -> pSpectrumSpd
      "temp" -> do
         temp <- flt
         return (sBlackBody temp)
      _ -> fail ("unknown spectrum type " ++ t)

namedSpectrum :: String -> JobParser Spectrum
namedSpectrum n = string n >> ws >> pSpectrum
      
pSpectrumSpd :: JobParser Spectrum
pSpectrumSpd = do
   spd <- between (char '{' >> optional ws) (optional ws >> char '}') ss
   return (fromSpd (mkSpd spd)) where
      ss = sepBy1 s (char ',' >> optional ws)
      s = do l <- flt; v <- ws >> flt; optional ws; return (l, v)

pBlock :: JobParser a -> JobParser a
pBlock = between (char '{' >> optional ws) (optional ws >> char '}')
      
namedBlock :: JobParser a -> String -> JobParser a
namedBlock p n = optional ws >> string n >> optional ws >> pBlock p
   
namedFloat :: String -> JobParser Flt
namedFloat n = do
   _ <- string n <|> fail ("expected " ++ n)
   ws
   flt <|> fail ("cannot parse " ++ n ++ " value")
   
namedInt :: String -> JobParser Int
namedInt n = do
   _ <- string n
   _ <- spaces
   integ <|> fail ("cannot parse " ++ n ++ " value")

-- | parse an integer
integ :: (Monad m) => (ParsecT String u m) Int
{-# INLINE integ #-}
integ = do
   x <- many1 digit
   return (read x)

--------------------------------------------------------------------------------
-- External Resources
--------------------------------------------------------------------------------

resolveFile :: FilePath -> JobParser FilePath
resolveFile n = do
   s <- getState
   return $ (basePath s) `FP.combine` n

-- | reads a file into a lazy ByteString
readFileBS :: FilePath -> JobParser BS.ByteString
readFileBS n = resolveFile n >>= \f -> liftIO $ do
   putStrLn $ "Reading " ++ n
   BS.readFile f

-- | reads a file into a String
readFileString :: FilePath -> JobParser String
readFileString n = resolveFile n >>= \f -> liftIO $ do
   putStrLn $ "Reading " ++ n
   readFile f
   