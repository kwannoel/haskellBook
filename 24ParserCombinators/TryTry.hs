module TryTry where

import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))

fractions :: [String]
fractions = ["1/10", "1/1", "2/3"] -- Add fringe cases of 1/0, 0/1, 2/2, stacked fractions (e.g. 2/3/4)

parseFractionAndDec :: Parser Rational
parseFractionAndDec = do
  numerator <- decimal
  char '/'
  char '.'
  denominator <- decimal
  return (numerator % denominator)

parseFractionOrDec :: Parser Rational
parseFractionOrDec = do
  numerator <- decimal
  char '/' <|> char '.'
  denominator <- decimal
  return (numerator % denominator)

main :: IO ()
main = do
  let p f i = parseString f mempty i
  foldMap (print . p parseFractionAndDec) fractions
  foldMap (print . p parseFractionOrDec) fractions
