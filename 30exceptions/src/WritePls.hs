module Main where

main = do
  writeFile "aaa" "hi"
  putStrLn "wrote to file"

-- to output:
-- > stack ghc -- <filename> -o <output file name>
-- to execute:
-- > ./<output file name>


