
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Bling.SamplingNew (
  ) where

{-
type Sample1D s = s Float
type Sample2D s = s (Float, Float)

--class Monad m => Sample1D m where
--  get1d :: a -> m Float

--class Monad m => Sample2D m a where
--  get2d :: a -> m (Float, Float)

-- a computation which uses sampled values to produce a result
-- newtype Sampled s a = Sampled { runSampled :: s -> a }

-- type Sampled r a = r -> a

class Monad a => Sampled a where
  runSampled :: g a -> a
  
class Monad g => SampleGet g where
  getSample1d :: g (Sample1D r)
  getSample2d :: g (Sample2D r)
--  mkSampled   :: g a -> a
-}

--class Monad s => Sampler s where
--  data Sampled s :: * -> *
--  getSample1d :: s (Sampled s Float)
--  getSample2d :: (Sampled s (Float, Float))
--  evs :: s -> Sampled s a -> a

newtype Sampled s m a = Sampled { runSampled :: s -> m a }

class Monad m => Sampler m where
  getSample1d :: m (Sampled s m Float)
  getSample2d :: m (Sampled s m (Float, Float))
  

test r = do
  s1d <- getSample1d -- gives a monadic action that when run provides a Float
--  s2d <- getSample2d -- gives a monadic action that when run provides a (Float, Float)
  
  return $ Sampled $ do
    s1d >>= \x -> if (x > 0.5)
                  then return $ Just "yo"
                  else return Nothing
