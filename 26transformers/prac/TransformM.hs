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

instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m
      => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  f <*> a = EitherT $ (<*>) <$> runEitherT f <*> runEitherT a

instance Monad m
      => Monad (EitherT e m) where
  return = pure
  v >>= f =
    EitherT $ runEitherT v >>=
    \lr -> case lr of
      Left x -> return (Left x)
      Right y -> runEitherT . f $ y

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT e = EitherT $ swapEither <$> runEitherT e where
  swapEither :: Either e a -> Either a e
  swapEither (Left a) = Right a
  swapEither (Right a) = Left a
