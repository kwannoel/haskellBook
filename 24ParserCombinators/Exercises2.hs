module Exercises2 where

import Text.Trifecta
import Control.Applicative
import Data.Traversable
import Data.Foldable

num :: String
num = ['0'..'9']

parseDigit :: Parser Char
parseDigit =
  foldr1 (<|>) $ fmap char num

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  pre <- string "-" <|> return ""
  int <- base10Integer
  if pre == "-" then return (-int) else return int

testCases :: [String]
testCases = [
  "123",
  "111",
  "1",
  "abc",
  "1123ab",
  "-1111"]

main :: IO ()
main = do
  let p pa i = parseString pa mempty i
  traverse_ (print . p parseDigit) testCases
  putStrLn "\n\n\n\n"
  traverse_ (print . p base10Integer) testCases
  putStrLn "\n\n\n\n"
  traverse_ (print . p base10Integer') testCases
