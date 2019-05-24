module Exercises where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

newtype Identity a = Identity a deriving Show

rDec :: Num a => Reader a a
rDec = reader (`subtract` 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> putStrLn ("hi: " ++ show x) >> return (x + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> putStrLn ("hi: " ++ show s) >> return (show s, s + 1)

main :: (Num a, Show a) => IO (String, a)
main = do
  print $ runReader rDec 1
  print $ fmap (runReader rDec) [1..10]
  print $ runReaderT rShow 1
  print $ fmap (runReaderT rShow) [1..10]
  runReaderT rPrintAndInc 1
  runStateT sPrintIncAccum 10
