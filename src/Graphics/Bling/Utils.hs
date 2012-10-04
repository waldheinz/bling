
module Graphics.Bling.Utils (

   -- * NFData Wrappers for Vectors

   NFUVector, unNFUVector, mkNFUVector,
   NFBVector, unNFBVector, mkNFBVector,
   
   -- * Growing Vectors
   
   GrowVec, gvAdd, gvNew, gvLength, gvFreeze,
   
   -- * Debugging Tools
   
   debugRay
   
   ) where

import Graphics.Bling.Math

import Control.DeepSeq
import Control.Monad.Primitive
import Data.List (intersperse)
import Data.Primitive.MutVar
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import Debug.Trace

--------------------------------------------------------------------------------
-- Debugging Tools
--------------------------------------------------------------------------------

debugRay :: Float -> Ray -> a -> a
debugRay sc (Ray o d _ _) a = (flip trace) a (str o 1 ++ " " ++ str d sc) where
   str v sc' = concat $ (intersperse " ") $ map (\ve -> show $ sc' * ve v) [vx, vy, vz]
   
--------------------------------------------------------------------------------
-- NFData wrappers for Vectors
--------------------------------------------------------------------------------

newtype NFUVector a = NFUVector { unNFUVector :: UV.Vector a }

mkNFUVector :: UV.Vector a -> NFUVector a
mkNFUVector = NFUVector

instance (NFData a, UV.Unbox a) => NFData (NFUVector a) where
   rnf (NFUVector v) = rnf $ UV.toList v
   {-# INLINE rnf #-}
   

newtype NFBVector a = NFBVector { unNFBVector :: BV.Vector a }

mkNFBVector :: BV.Vector a -> NFBVector a
mkNFBVector = NFBVector

instance (NFData a) => NFData (NFBVector a) where
   rnf (NFBVector v) = rnf $ BV.toList v
   {-# INLINE rnf #-}

--------------------------------------------------------------------------------
-- GrowVec
--------------------------------------------------------------------------------

data GrowVec v s a = GV ! (MutVar s (v s a)) ! (MutVar s Int)

gvAdd :: (PrimMonad m, MV.MVector v a) => GrowVec v (PrimState m) a -> a -> m ()
{-# INLINE gvAdd #-}
gvAdd (GV vr cntr) e = do
   v <- readMutVar vr
   cnt <- readMutVar cntr
   
   let l = MV.length v
   v' <- if (l < (cnt + 1))
      then do
         x <- MV.unsafeGrow v l
         writeMutVar vr x
         return x
      else return v
   
   seq e $ MV.unsafeWrite v' cnt e
   writeMutVar cntr $ let x = (cnt + 1) in seq x x

gvNew :: (PrimMonad m, MV.MVector v a) => m (GrowVec v (PrimState m) a)
{-# INLINE gvNew #-}
gvNew = do
   vr <- MV.new 64 >>= newMutVar
   cr <- newMutVar 0
   return $! GV vr cr

gvLength :: (PrimMonad m) => GrowVec v (PrimState m) a -> m Int
{-# INLINE gvLength #-}
gvLength (GV _ cr) = readMutVar cr

gvFreeze :: (V.Vector v a, PrimMonad m) => GrowVec (V.Mutable v) (PrimState m) a -> m (v a)
{-# INLINE gvFreeze #-}
gvFreeze (GV vr cr) = do
   v <- readMutVar vr
   c <- readMutVar cr
   V.freeze (MV.unsafeTake c v)

