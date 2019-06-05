module Criteria where

import Criterion.Main

infixl 9 !?

_ !? n | n < 0 = Nothing -- not
[] !? _ = Nothing -- not
(x:_) !? 0 = Just x -- not
(_:xs) !? n = xs !? (n-1) -- not a data constructor

myList :: [Int]
myList = [1..9999]

-- Here we compare 2 things whnf of our safe ver vs unsafe (!!) ver
-- only WHNF not NF is needed as they do not return a data constructor
main :: IO ()
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998 -- observe here we pass arguments to prevent pointfree sharing
  , bench "index list maybe index 9999"
    $ whnf (myList !?) 9998 -- same here
  ]

-- using a sample
-- [1,2,3] ?! 2

-- (_: [2,3]) !? 2 = [2,3] !? (2-1)

-- [2,3] !? 1
-- . . .

-- [3] !? 0 matches Just Case

-- we happen to know x is 3, but it'll get thunked if it wasn't eval upon list cons
