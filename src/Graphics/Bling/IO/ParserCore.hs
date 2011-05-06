
module Graphics.Bling.IO.ParserCore (

   -- * Data Types

   JobParser, PState(..), nextId,
   
   -- * Core Parsing Primitives
   flt, ws, pVec, pSpectrum, namedBlock, namedInt, namedFloat,
   namedVector, integ
   
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Camera
import Graphics.Bling.Filter
import Graphics.Bling.Integrator
import Graphics.Bling.Light
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Reflection
import Graphics.Bling.Sampling
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

type JobParser a = GenParser Char PState a

data PState = PState {
   resX :: Int,
   resY :: Int,
   pxFilter :: Filter, -- ^ the pixel filtering function
   camera :: Camera,
   surfaceIntegrator :: AnySurfaceIntegrator,
   transform :: Transform,
   material :: Material,
   sampler :: AnySampler,
   emit :: Maybe Spectrum, -- ^ the emission for the next primitives
   lights :: [Light],
   prims :: [AnyPrim],
   currId :: Int
   }

nextId :: JobParser Int
nextId = do
   s <- getState
   let nid = currId s
   setState s { currId = nid + 1 }
   return $ nid

comment :: JobParser ()
comment = do
   char '#' >> many (noneOf "\n") >> char '\n' >> return () <?> "comment"

-- | skips over whitespace and comments
ws :: JobParser ()
ws = many1 (choice [space >> return (), comment]) >> return ()

-- | parse a floating point number
flt :: JobParser Flt
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
      
pSpectrumSpd :: JobParser Spectrum
pSpectrumSpd = do
   spd <- between (char '{' >> optional ws) (optional ws >> char '}') ss
   return (fromSpd (mkSpd spd)) where
      ss = sepBy1 s (char ',' >> optional ws)
      s = do l <- flt; v <- ws >> flt; optional ws; return (l, v)
      
namedBlock :: JobParser a -> String -> JobParser a
namedBlock p n = do
   string n >> optional ws
   between (char '{' >> optional ws) (optional ws >> char '}') p
   
namedFloat :: String -> JobParser Flt
namedFloat n = do
   _ <- string n >> ws
   flt <|> fail ("cannot parse " ++ n ++ " value")
   
namedInt :: String -> JobParser Int
namedInt n = do
   _ <- string n
   _ <- spaces
   integ <|> fail ("cannot parse " ++ n ++ " value")

-- | parse an integer
integ :: JobParser Int
integ = do
   x <- many1 digit
   return (read x)
