
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Bling.SamplingNew (
  main
  ) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Primitive
import System.Random.MWC as MWC

newtype Stratified m a = Stratified (StateT (Int, Int) m a)
                       deriving ( Functor, Applicative, Monad, MonadState (Int, Int) )

type SRunState m = ([Float], Gen (PrimState m))

newtype StratRun m a = StratRun (StateT (SRunState m) m a)
                       deriving ( Functor, Applicative, Monad, MonadState (SRunState m) )

instance MonadTrans StratRun where
  lift c = StratRun $ lift c

mk1d :: (Monad m, PrimMonad r, MonadState (SRunState r) (StratRun r)) => Stratified m (StratRun r Float)
mk1d = do
  (n1d, n2d) <- get
  put (n1d + 1, n2d)
  
  return $ do
    fs <- gets fst
    if length fs < n1d
      then return (fs !! n1d)
      else gets snd >>= (lift . MWC.uniform)

test2d :: (Monad m, PrimMonad r, MonadState (SRunState r) (StratRun r)) => Stratified m (StratRun r (Float, Float))
test2d = do
  f1 <- mk1d
  f2 <- mk1d
  
  return $ do
    v1 <- f1
    v2 <- f2
    return (v1, v2)

runStratified :: (PrimMonad m) => Stratified m a -> m a
runStratified (Stratified c) = do
  (StratRun x, (n1d, n2d)) <- runStateT c (0, 0)
  gen <- MWC.create
  
  --evalStateT x ([0.5 :: Float], undefined)
  undefined

main :: IO ()
main = do
  
  return ()
