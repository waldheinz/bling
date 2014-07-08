
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Bling.SamplingNew ( main ) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Primitive
import System.Random.MWC as MWC

-- allows to generate stratified computations
type SGenState = (Int, Int)

newtype StratGen s m a = StratGen (StateT s m a)
                       deriving ( Functor, Applicative, Monad, MonadState s, MonadTrans )

-- state allowing to evaluate stratified computations
type SRunState m = ([Float], Gen (PrimState m))

-- allows to evaluate stratified computations
newtype StratRun s m a = StratRun (StateT s m a)
                       deriving ( Functor, Applicative, Monad, MonadState s )

type Sampled g r m a = StratGen g m (StratRun r m a)

-- a stratified computation, constructed in StratGen and evaluated in StratRun
type Stratified m a = Sampled SGenState (SRunState m) m a

mk1d :: PrimMonad m => Stratified m Float
mk1d = do
  (n1d, n2d) <- get
  put (n1d + 1, n2d)
  
  return $ StratRun $ do
    fs <- gets fst
    if length fs > n1d
      then return (fs !! n1d)
      else gets snd >>= lift . MWC.uniform

mk2d :: (Functor m, PrimMonad m) => Stratified m (Float, Float)
mk2d = mk1d >>= \f1 -> mk1d >>= \f2 ->
  return $ (,) <$> f1 <*> f2
  
runStratified
  :: (PrimMonad m)
  => Int            -- ^ number of samples in one dimension
  -> Int            -- ^ number of samples in other dimension
  -> Stratified m a -- ^ computation to evaluate
  -> m [a]          -- ^ the values produced, a list of nx * ny values
runStratified nx ny (StratGen c) = do
  (StratRun x, (n1d, n2d)) <- runStateT c (0, 0)
  -- let's just pretend I'd use n1d and n2d to actually
  -- compute stratified samples
  gen <- MWC.create
  replicateM (nx * ny) $ evalStateT x ([], gen)
  
-- estimate Pi by Monte Carlo sampling
mcPi :: (Functor m, PrimMonad m) => Sampled g r m Float
mcPi = do
  v <- mk2d
  return $ v >>= \(x, y) -> return $ if x * x + y * y < 1 then 4 else 0

main :: IO ()
main = do
  vs <- (runStratified 100 100 mcPi) :: IO [Float]
  print $ sum vs / (fromIntegral $ length vs)
