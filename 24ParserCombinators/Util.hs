module Util where

createNZeros :: Int -> String
createNZeros n = take n ['0','0'..]

precedeZeros :: Int -> String -> String
precedeZeros n str | n < length str || n < 0 = str
                   | otherwise = createNZeros (n - length str) ++ str

