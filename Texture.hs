{-# LANGUAGE ExistentialQuantification #-}

module Texture where

import Debug.Trace

import Color
import Geometry

class Texture t where
   evalTexture :: t -> DifferentialGeometry -> Spectrum
   
data Constant = Constant Spectrum

instance Texture Constant where
   evalTexture (Constant r) _ = r
   
data GraphPaper = GraphPaper {
   graphPaperLineWidth :: Float,
   graphPaper :: Spectrum,
   graphLines :: Spectrum
   }

instance Texture GraphPaper where
   evalTexture (GraphPaper lw p l) (DifferentialGeometry (x, _, z) _)
      | x' < lo || z' < lo || x' > hi || z' > hi = l
      | otherwise = p
      where
            x' = abs x''
            z' = abs z''
            (_, x'') = properFraction x
            (_, z'') = properFraction z
            lo = lw / 2
            hi = 1.0 - lo
            
data AnyTexture = forall a. Texture a => MkAnyTexture a

instance Texture AnyTexture where
   evalTexture (MkAnyTexture t) = evalTexture t