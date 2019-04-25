module Exercises5 where

import Data.Foldable
import Data.Word
import Text.Trifecta
import Control.Applicative
import Control.Applicative.Combinators hiding (count)

newtype IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

testCases :: [String]
testCases = [
  "172.16.254.1",
  "204.120.0.15"
            ]

rangeDigit3 :: Parser String
rangeDigit3 = count' 1 3 digit

{-
count' :: Alternative m => Int -> Int -> m a -> m [a]
count' m' n' p = go m' n'
  where
    go !m !n
      | n <= 0 || m > n = pure []
      | m > 0           = liftA2 (:) p (go (m - 1) (n - 1))
      | otherwise       = liftA2 (:) p (go 0 (n - 1)) <|> pure []
-}

{-
 also works:
rangeDigit3 =
      try (token (count 3 digit))
  <|> try (token (count 2 digit))
  <|> token (fmap (: []) digit)

-}

seperateIP :: Parser [String]
seperateIP = do
  i1 <- rangeDigit3
  _ <- char '.'
  i2 <- rangeDigit3
  _ <- char '.'
  i3 <- rangeDigit3
  _ <- char '.'
  i4 <- rangeDigit3
  return [i1, i2, i3, i4]

convertToWord32 :: [String] -> Word32
convertToWord32 [] = 0
convertToWord32 (x:xs) = read x * 256 ^ length xs + convertToWord32 xs

main :: IO ()
main = do
  let p pa = parseString pa mempty
  print $ p seperateIP "123.123.123.122"
  traverse_ (print . p seperateIP) testCases
  traverse_ (print . convertToWord32 . fold . p seperateIP) testCases
