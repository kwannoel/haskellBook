module Exercises3 where

import Text.Trifecta
import Data.Foldable

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser [String]
parsePhone = do
  _ <- optional $ string "1-" <?> "zeroth set"
  _ <- optional $ string "("
  npa <- repeatX 3 digit <?> "first set"
  _ <- optional $ string ") "
  _ <- optional $ string "-"
  e <- repeatX 3 digit <?> "snd set"
  _ <- optional $ string "-"
  ln <- repeatX 4 digit <?> "thrd set"
  return [npa, e, ln]
--  return $ PhoneNumber (read npa) (read e) (read ln)

repeatX :: Monad m => Int -> m a -> m [a]
repeatX n ma = go [] n ma where
  go results n ma
    | n == 0 = return results
    | n < 0 = return results
    | otherwise = ma >>= (\x -> go (results ++ [x]) (n-1) ma)

testCases :: [String]
testCases = ["123-456-7890", "1234567890", "(123) 456-7890", "1-123-456-7890"]

main :: IO ()
main = do
  traverse_ (print . parseString parsePhone mempty) testCases
