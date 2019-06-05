module CriteriaOpt where

import Criterion.Main

infixl 9 !?

{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
      (\x r k ->
         case k of
           0 -> Just x
           _ -> r (k - 1))
      (const Nothing) xs n

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

-- When to use whnf / nf?
-- whnf: Example: going through db and knowing how long it takes to traverse
-- nf: Example: how long it takes to print out results from db

-- another example: benchmarking map on a lazy list:
-- map (+1) [..] here, map is only eval to whnf
-- i.e. (_:_) and thus... this does not compute the actual time taken
