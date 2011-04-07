
module Graphics.Bling.IO.ParserCore (

   -- * Data Types

   JobParser, PState(..),
   
   -- * Core Parsing Primitives
   flt, ws, pVec
   
   ) where

import Text.ParserCombinators.Parsec

import Graphics.Bling.Camera
import Graphics.Bling.Filter
import Graphics.Bling.Light
import Graphics.Bling.Material
import Graphics.Bling.Math
import Graphics.Bling.Primitive
import Graphics.Bling.Spectrum
import Graphics.Bling.Transform

type JobParser a = GenParser Char PState a

data PState = PState {
   resX :: Int,
   resY :: Int,
   pxFilter :: Filter, -- ^ the pixel filtering function
   camera :: Camera,
   transform :: Transform,
   material :: Material,
   _spp :: Int,
   emit :: Maybe Spectrum, -- ^ the emission for the next primitives
   lights :: [Light],
   prims :: [AnyPrim]
   }

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
  d <- try (char '.' >> try (many digit))
  return $ sign * read (i++"."++d)

-- | parse a vector
pVec :: JobParser Vector
pVec = do
   x <- flt
   y <- ws >> flt
   z <- ws >> flt
   return (Vector x y z)
