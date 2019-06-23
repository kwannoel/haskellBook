module HowToMVar where

import Control.Concurrent
import System.IO.Unsafe

main :: IO ()
main = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero

myData :: MVar Int
myData = unsafePerformIO newEmptyMVar

-- type of unsafePerformIO is IO a -> a

main2 :: IO ()
main2 = do
  putMVar myData 0
  zero <- takeMVar myData
  print zero

