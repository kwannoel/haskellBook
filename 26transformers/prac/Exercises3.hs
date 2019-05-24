{-# LANGUAGE OverloadedStrings #-}
module Exercises3 where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Data.String

data Config =
  Config {
    -- that's one, one click!
    -- two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = let i = maybe 0 (+1) $ M.lookup k m
                in (M.insert k i m, i)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key" :: Handler Text
    -- lift $ ReaderT (\a -> let m = modifyIORef (counts a) $ fst . bumpBoomp unprefixed
    --                       in  m)
    Config c p <- lift $ ReaderT return
    let changedStore :: IO Integer
        changedStore =  c `atomicModifyIORef`  bumpBoomp unprefixed
        -- newStore :: Handler Config
        -- newStore = undefined
        key' = mappend p unprefixed
    newInteger <- liftIO changedStore :: Handler Integer
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger ++ show key'
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ fromString prefixArg
      runR r = runReaderT r config
  scottyT 3000 runR app
