module CAF where

incdInts :: [Integer]
incdInts = map (+1) [1..]

main :: IO ()
main = do
  print (incdInts !! 1000)
  print (incdInts !! 9001)
  print (incdInts !! 90010)
  print (incdInts !! 9001000)
  print (incdInts !! 9501000)
  print (incdInts !! 9901000)

-- incdInts is its own CAF, memory alloc is 100%
-- this is because mapping over an inf list is a top-level value that can be shared throughout module

-- CAFs:
-- values
-- partially applied functions w/o named args
-- fully applied funcs such as incdInts

-- can be memory intensive
-- find the biggest CAF and kill it if needed

-- we also can pointful incdInts which is pointfree atm
