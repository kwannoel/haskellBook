module LiftMore where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a)}

instance MonadTrans (EitherT e) where
  lift = EitherT . (Right <$>)

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma


