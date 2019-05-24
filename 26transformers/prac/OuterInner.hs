module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

exceptUnwrap :: ReaderT () IO (Either String (Maybe Int))
exceptUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT exceptUnwrap

-- base monad is structurally outer most
-- i.e. type MyType a = IO [Maybe a]

embedded' :: MaybeT (ExceptT String
                            (ReaderT () IO))
                   Int
  -- b0 -> Either String (Maybe Int)
embedded' = MaybeT . ExceptT . ReaderT $ (return <$> const (Right (Just 1)))
