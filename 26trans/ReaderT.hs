{-# LANGUAGE InstanceSigs #-}
module ReaderT where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m)
      => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  rf <*> ra = ReaderT $ (<*>) <$> runReaderT rf <*> runReaderT ra

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  rf >>= f = ReaderT $
    \r -> runReaderT rf r >>= \ma -> (runReaderT . f) ma r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT r m) where
  fmap f (StateT fs) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) fs

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  --  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> a = StateT $ \s -> let mfs1 = runStateT f s
                            -- mfs1 :: m (a -> b, s)
                               sma = runStateT a
                            -- sma :: s -> m (a, s)
                           in mfs1 >>= \(f1, s1) ->
                                         let f' (ax, sx) = (f1 ax, sx)
                                         in fmap f' (sma s1)

instance (Monad m) => Monad (StateT r m) where
  return = pure
  -- (>>=) :: StateT r m a -> (a -> StateT r m b) -> StateT r m b
  StateT a >>= fs =
    StateT $
    -- a :: s -> m (a, s)
    -- f' :: a -> (s -> m (b, s))
    -- as :: m (a, s)
    -- f :: (a, s) -> m (b, s)
      \s -> let as = a s
                f' = fmap runStateT fs
                f (a1, s2) = f' a1 s2
            in as >>= f

main :: IO ()
main = do
  quickBatch $ functor (nope :: ReaderT r m (String, Integer, String))
  quickBatch $ applicative (nope :: ReaderT r m (String, Integer, String))
