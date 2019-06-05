module CoreDump where

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True -> 1

forceNothing _ = 0
