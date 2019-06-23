{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (ord, chr)
import qualified System.Environment as SE
import qualified System.IO as SI
import Test.QuickCheck

decoded :: String
decoded = "MEET AT DAWN"

sampleKw :: String
sampleKw = "ALLY"

generateEncoding :: String -> String -> String
generateEncoding secret phrase = go 0 secret phrase ""
  where go _ "" p _ = p
        go _ _ "" e = e
        go c s (x:xs) e = case x of
          ' ' -> go c s xs (e ++ " ")
          _   -> go ((c + 1) `mod` 4) s xs (e ++ [s !! c])

-- The assumption is 'A' == 0, we are only dealing with set of ['A'..'Z']
convertToNum :: Char -> Int
convertToNum = ord

convertFromNum :: Int -> Char
convertFromNum = chr

shiftChar :: Char -> Char -> Char
shiftChar encodingCharacter c = convertFromNum $ (+65) $ (convertToNum c + convertToNum encodingCharacter) `mod` 26

unshiftChar :: Char -> Char -> Char
unshiftChar encodingCharacter c = convertFromNum $ (+65) $ (convertToNum c - convertToNum encodingCharacter) `mod` 26

encodeChar :: (Char -> Char -> Char) -> Char -> Char -> Char
encodeChar encodingF encodingCharacter c | c `elem` ['A'..'Z'] = encodingF encodingCharacter c
                               | otherwise = c

encodeString :: String -> String -> String
encodeString = zipWith $ encodeChar shiftChar

decodeString :: String -> String -> String
decodeString = zipWith $ encodeChar unshiftChar

encodeWithSecret :: String -> String -> String
encodeWithSecret kw str = encodeString (generateEncoding kw str) str

decodeWithSecret :: String -> String -> String
decodeWithSecret kw str = decodeString (generateEncoding kw str) str

unitTest :: IO ()
unitTest = do
  print "Test encoding"
  print $ generateEncoding sampleKw decoded == "ALLY AL LYAL"
  print "Test conversion"
  print $ shiftChar 'A' 'A' == 'A'
  print $ shiftChar 'B' 'C' == 'D'
  print $ encodeString "ALLY AL LYAL" decoded == "MPPR AE OYWY"
  print $ encodeWithSecret "ALLY" decoded == "MPPR AE OYWY"
  print $ decodeWithSecret "ALLY" "MPPR AE OYWY" == decoded

main :: IO ()
main = do
  -- get arguments
  args <- SE.getArgs
  -- get file
  putStrLn "Enter your input file path:"
  filePathI <- SI.hGetLine SI.stdin
  SI.hWaitForInput SI.stderr 1000
  text <- readFile filePathI
  putStrLn text
  coded <- case head args of
    "d" -> return $ encodeWithSecret "ABCD" text
    "e" -> return $ decodeWithSecret "ABCD" text
    _ -> error "Invalid options"
  -- return file
  -- Note here we cannot write to the same file because!! it is currently being read due after being forced by writeFile!! As a result it attempts to read and write at the same time.
  putStrLn "Enter your output file path:"
  filePathO <- SI.hGetLine SI.stdin
  writeFile filePathO coded
