module Main where

-- > touch zzz
-- > chmod 400 zzz

main = do
  writeFile "zzz" "hi"
  putStrLn "wrote to file"
