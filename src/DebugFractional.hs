
module DebugFractional where

import Control.Exception

data DebugFractional = DebugFractional Float deriving (Show, Eq)

instance Num DebugFractional where
   
   DebugFractional a + DebugFractional b =
      assert ((not $ isNaN a) && (not $ isNaN b)) DebugFractional (a + b)
      
   DebugFractional a * DebugFractional b =
      assert ((not $ isNaN a) && (not $ isNaN b)) DebugFractional (a * b)
      
   abs (DebugFractional a) = DebugFractional $ abs a
   signum (DebugFractional a) = DebugFractional $ signum a
   fromInteger a = DebugFractional $ fromInteger a
   
instance Fractional DebugFractional where
   fromRational a = DebugFractional $ fromRational a
   DebugFractional a / DebugFractional b
      | b == 0 = error "division by zero"
      | isNaN a = error "diviging a NaN"
      | otherwise = DebugFractional (a / b)
   
instance Ord DebugFractional where
   DebugFractional a <= DebugFractional b
      | (isNaN a) || (isNaN b) = error "comparing with NaN"
      | otherwise = a <= b
   
instance Real DebugFractional where
   toRational (DebugFractional a)
      | isNaN a = error "toRational on NaN"
      | otherwise = toRational a
   
instance RealFrac DebugFractional where
   properFraction (DebugFractional a)
      | isNaN a = error "properFraction on NaN"
      | otherwise = (b, DebugFractional c) where
         (b, c) = properFraction a
