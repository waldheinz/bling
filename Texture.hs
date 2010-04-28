{-# LANGUAGE ExistentialQuantification #-}

module Texture where

import Debug.Trace

import Color
import Geometry

class Texture t where
   evalTexture :: t -> DifferentialGeometry -> Spectrum
   
data GraphPaper = GraphPaper {
   graphPaper :: Spectrum,
   graphLines :: Spectrum
   }

instance Texture GraphPaper where
   evalTexture (GraphPaper p l) (DifferentialGeometry (x, _, z) _)
      | x' < 0.05 || z' < 0.05 || x' > 0.95 || z' > 0.95 = l
      | otherwise = p
      where
            x' = abs x''
            z' = abs z''
            (_, x'') = properFraction x
            (_, z'') = properFraction z
            
data AnyTexture = forall a. Texture a => MkAnyTexture a

instance Texture AnyTexture where
   evalTexture (MkAnyTexture t) = evalTexture t