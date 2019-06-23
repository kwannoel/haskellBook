module WhatHappens where

import Control.Concurrent
import Debug.Trace

myData :: IO (MVar Int)
myData = newEmptyMVar

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

main :: IO ()
main = do
  mv <- myData -- here is a different MVar
  putMVar mv 0
  mv' <- myData -- from here
  zero <- takeMVar mv'
  print zero

-- Reason being newEmptyMVar is a recipe for a producing an empty MVar without sharing
-- As a result we try to take from an empty MVar in the second `take` step.

-- i.e. put take take

-- Possible to share the MVar, but this has to be done explicitly

mainTrace :: IO ()
mainTrace = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

-- purity -> lambda calculus
-- impure functional languages more typical --> imperative effectful programs could be embedded
-- haskell -> stick to lambda calculus -> simple core language
-- nested lambdas to order and encapsulate effects

-- referential transparency --> any function when given the same inputs returns the same result
