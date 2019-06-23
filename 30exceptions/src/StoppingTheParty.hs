module Main where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9]
    then throwIO DivideByZero -- ArithException caught
    else throwIO StackOverflow -- This is not caught by the handler

main :: IO ()
main = forever $ do
  let tryS :: IO ()
           -> IO (Either ArithException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!"
  -- ms
  threadDelay (1 * 1000000)
