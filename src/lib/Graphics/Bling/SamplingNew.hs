
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Bling.SamplingNew (
--  main
  ) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Writer.Class as WC
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Primitive
import System.Random.MWC as MWC

import Debug.Trace

newtype StratGen m a = StratGen (StateT (Int, Int) m a)
                       deriving ( Functor, Applicative, Monad, MonadState (Int, Int), MonadTrans )

type SRunState m = ([Float], Gen (PrimState m))

newtype StratRun m a = StratRun (StateT (SRunState m) m a)
                       deriving ( Functor, Applicative, Monad, MonadState (SRunState m) )

instance MonadTrans StratRun where
  lift c = StratRun $ lift c

newtype Stratified m a = Stratified { unStratified :: StratGen m (StratRun m a) }

mk1d :: PrimMonad m => StratGen m (StratRun m Float)
mk1d = do
  (n1d, n2d) <- get
  put (n1d + 1, n2d)
  
  return $ StratRun $ do
    fs <- gets fst
    if length fs < n1d
      then return (fs !! n1d)
      else gets snd >>= lift . MWC.uniform

mk2d :: PrimMonad m => StratGen m (StratRun m (Float, Float))
mk2d = do
  f1 <- mk1d
  f2 <- mk1d
  
  return $ do
    v1 <- f1
    v2 <- f2
    return (v1, v2)

runStratified
  :: (PrimMonad m, MonadWriter a m)
  => Int                              -- ^ number of samples
  -> StratGen m (StratRun m a)
  -> m () -- ^ computation to evaluate
runStratified nsamples (StratGen c) = do
  (StratRun x, (n1d, n2d)) <- runStateT c (0, 0)
  gen <- MWC.create
  replicateM_ nsamples $
    evalStateT x ([0.5], gen) >>= WC.tell
  
main :: IO ()
main = do
  let
    mcPi = do
      v <- mk2d
      return $ v >>= \(x, y) -> return $ if x * x + y * y < 1 then 1 else (0 :: Float)
    
  vs <- (runStratified 10 $ mcPi)
  return ()
