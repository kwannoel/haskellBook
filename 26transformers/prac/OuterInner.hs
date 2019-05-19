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


