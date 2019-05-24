{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Except
import Control.Monad (liftM)
import Control.Monad.Trans.Reader

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT . (ExceptT . liftM Right) . liftReaderT . lift) $ putStrLn "hello"
    html $
      mconcat ["<h1>Scotty, ",
              beam,
              " me up!</h1>"]

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

--  lift = ReaderT . const
--  ReaderT :: (r -> m a) -> ReaderT r m a
--  const :: a -> b -> a
--  const :: m a -> (b -> m a)
--  ReaderT . const :: ma -> ReaderT r m a

