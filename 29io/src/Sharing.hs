module Sharing where

-- IO turns off a lot of the sharing we talked about in the nonstrictness chapter.

-- why sharing is disabled:
-- If a function is going to be evaluated at all it will result in a value of a certain type

-- In IO however, values of type IO a are not an a,
-- They are a description of how you might get an a.
-- Think about this like the StateT wrapper.
-- StateT hides a function beneath.
-- describing io actions does not perform them, a recipe for a cake does not give you a cake.

-- Example:

-- getCurrentTime :: IO UTCTime

-- If sharing wasn't disabled, this would result in the same value

-- criterion whnf and nf functions:
-- turn off sharing for these so they get evaluated over and over again
-- criterion evaluates the same function multiple times

-- IO variants of these functions do not require these function applications in order to disable sharing
-- e.g.
-- whnf :: (a -> b) -> a -> Benchmarkable
-- whnfIO :: IO a -> Benchmarkable

-- Take note IO doesn't turn off sharing everywhere
-- Doesnt make sense because main is always in IO

-- MVar can only hold one value at a time
