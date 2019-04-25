-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Exercises4 where

import Control.Applicative
import System.Random
import Data.Foldable
import Data.List (intercalate)
import Text.RawString.QQ
import Text.Trifecta
import Test.QuickCheck hiding (Result, Success)

testCases :: String
testCases = [r|

-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

newtype Log = Log [LogDets]

instance Show Log where
  show (Log []) = ""
  show (Log (x:xs)) = show x ++ show (Log xs)

instance Arbitrary Log where
  arbitrary = Log <$> listOf arbitrary

data LogDets = H Header | C Comment | R Remarks | NL | Space

instance Show LogDets where
  show (H h) = show h
  show (C c) = show c
  show (R re) = show re
  show NL = "\n"
  show Space = " "

instance Arbitrary LogDets where
  arbitrary = do
    h <- (arbitrary :: Gen Header)
    c <- (arbitrary :: Gen Comment)
    re <- (arbitrary :: Gen Remarks)
    oneof $ return <$> [H h, C c, R re, NL, Space]

newtype Header = Header Date

instance Show Header where
  show (Header dt) = "# " ++ show dt

instance Arbitrary Header where
  arbitrary = do
    d <- arbitrary
    return $ Header d

data Date = Date Year Month Day
type Year = Int
type Month = Int
type Day = Int

instance Show Date where
  show (Date yr mo da) = intercalate "-" [precedeZeros 4 . show $ yr,
                                          precedeZeros 2 . show $ mo,
                                          precedeZeros 2 . show $ da]

instance Arbitrary Date where
  arbitrary = do
    y <- choose (0, 9999)
    m <- choose (1, 12)
    d <- choose (1, 30)
    return $ Date y m d

-- Restrict 24 hrs also

data Time = Time HH MM
type HH = Int
type MM = Int

createNZeros :: Int -> String
createNZeros n = take n ['0'..]

precedeZeros :: Int -> String -> String
precedeZeros n str | n < length str || n < 0 = str
                   | otherwise = createNZeros (n - length str) ++ str

instance Show Time where
  show (Time hr minutes) = precedeZeros 2 (show hr)
                        ++ ":"
                        ++ precedeZeros 2 (show minutes)

instance Arbitrary Time where
  arbitrary = do
    h <- choose (0, 23)
    m <- choose (0, 59)
    return $ Time h m

data Remarks = Remarks Time String

instance Show Remarks where
  show (Remarks dt str) = show dt ++ " " ++ str

instance Arbitrary Remarks where
  arbitrary = do
    t <- arbitrary
    s <- listOf . elements $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ [' ']
    return $ Remarks t s

newtype Comment = Comment String

instance Show Comment where
  show (Comment str) = "-- " ++ str

instance Arbitrary Comment where
  arbitrary = do
    s <- listOf . elements $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ [' ']
    return $ Comment s

time :: Parser Time
time = do
  hh <- count 2 digit
  _  <- char ':'
  mm <- count 2 digit
  return (Time (read hh) (read mm))

date :: Parser Date
date = do
  yr <- count 4 digit
  _ <- char '-'
  mo <- count 2 digit
  _ <- char '-'
  da <- count 2 digit
  return $ Date (read yr) (read mo) (read da)

comment :: Parser Comment
comment = do
  _ <- string "-- "
  com <- many (noneOf "\n")
  return $ Comment com

remarks :: Parser Remarks
remarks = do
  ti   <- time
  _ <- char ' '
  rmks <- many (noneOf "\n")
  return $ Remarks ti rmks

header :: Parser Header
header = do
  _ <- string "# "
  dt <- date
  return $ Header dt

nl :: Parser LogDets
nl = string "\n" >> return NL

sp :: Parser LogDets
sp = char ' ' >> return Space

logDetsParse :: Parser LogDets
logDetsParse =
  choice [H <$> header,
          C <$> comment,
          R <$> remarks,
          nl,
          sp]

logParse :: Parser Log
logParse = Log <$> many logDetsParse

main :: IO ()
main = do
  let logParseProp' :: (Show a) => Parser a -> a -> ([String], Bool)
      logParseProp' pa a = let Success c = parseString pa mempty (show a)
                        in ([show a, show c], show c == show a)
      Success val = parseString logParse mempty testCases
      Log val' = val
      Success val'' = parseString logParse mempty $ show val
      Log val''' = val''
  print val'
  putStrLn  "START\n"
  print val'''
  putStrLn "END\n"
  putStrLn  "START\n"
  print (val' !! 2)
  putStrLn "END\n"
  print $ logParseProp' logParse val
  print $ logParseProp' comment (Comment " ")
  print $ logParseProp' time (Time 2 1)
  sample (arbitrary :: Gen Time)

logParseProp :: (Show a) => Parser a -> a -> Bool
logParseProp pa a = let Success c =
                          parseString pa mempty (show a)
                    in show c == show a

test :: IO ()
test = do
  print '1'
  quickCheck $ logParseProp header
  print '2'
  quickCheck $ logParseProp comment
  print '3'
  quickCheck $ logParseProp time
  print '4'
  quickCheck $ logParseProp remarks
  print '5'
  quickCheck $ logParseProp logDetsParse
  print '6'
  quickCheck $ logParseProp logParse
