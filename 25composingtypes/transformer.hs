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
  -- mma :: m (Maybe a)
  -- MaybeT :: m (Maybe a) -> MaybeT m a
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ 
