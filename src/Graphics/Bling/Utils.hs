
module Graphics.Bling.Utils (
   GrowVec, gvAdd, gvNew, gvLength, gvFreeze
   ) where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

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

