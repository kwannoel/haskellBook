{-# LANGUAGE InstanceSigs #-}
module TransformM where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m)
      => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
      => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  -- fab :: m (Maybe (a -> b))
  -- fmap (<*>) fab :: m (Maybe a -> Maybe b)
  -- mma :: m (Maybe a)
  -- MaybeT :: m (Maybe a) -> MaybeT m a
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b

  -- ma :: m (Maybe a)
  -- f :: a -> MaybeT m b
  -- runMaybeT . f :: a -> m (Maybe b)
  MaybeT ma >>= f = MaybeT $
    ma >>= \may -> case may of
                     Nothing -> return Nothing
                     Just a  -> runMaybeT . f $ a

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a)}

instance 
