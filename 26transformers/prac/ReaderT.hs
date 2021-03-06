{-# LANGUAGE InstanceSigs #-}
module ReaderT where

-- import Test.QuickCheck
-- import Test.QuickCheck.Classes
-- import Test.QuickCheck.Checkers

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

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

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  f <*> a = MaybeT $ (<*>) <$> runMaybeT f <*> runMaybeT a

instance Monad m => Monad (MaybeT m) where
  return = pure
  MaybeT a >>= f = MaybeT $
    a >>= \may -> case may of
                    Nothing -> return Nothing
                    Just a1 -> runMaybeT . f $ a1
    -- a :: m (Maybe a)
    -- f :: a -> MaybeT m a
    -- f' :: a -> m (Maybe a)

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  -- liftIO :: IO a -> m a
  liftIO = lift . liftIO

-- ReaderT r Maybe := ReaderT $ r -> Maybe a
-- MaybeT Reader r := MaybeT $ r -> Maybe a
