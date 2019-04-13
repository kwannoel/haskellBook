module Definitions where

newtype State s a =
  State { runState :: s -> (a, s) }

-- The newtype has to be isomorphic to the type it wraps
-- State and runState are isomorphic
