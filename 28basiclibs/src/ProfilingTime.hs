module ProfilingTime where

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 999999)
  putStrLn "g"

main :: IO ()
main = do
  f
  g

{-
$ stack ghc -- -prof -fprof-auto \
> -rtsopts -O2 profile.hs
./profile +RTS -P
cat profile.prof
-}

-- -prof enables profiling
-- -fprof-auto auto assign bindings not marked inline a cost center named after binding
-- ok to use for lightweight stuff / lower levels of precision
-- manually assign SCCs (cost center) otherwise
-- -rtsopts enables GHC RTS options to the binary
-- -02 is the highest level of program optimizations

-- results :: 91.2% time spent in g, 8.8% spent in f
