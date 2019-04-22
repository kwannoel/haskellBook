module Exercises where

import Data.Monoid
import           Control.Applicative
import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString            (ByteString)
import           Data.Foldable
import           Text.Parsec                (Parsec, parseTest)
import           Text.Trifecta              hiding (parseTest)

data NumberOrString =
    NOSS String
  | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

big = SemVer 2 1 1 [] []
little = SemVer 2 1 0 [] []


testCases :: [String]
testCases = [
  "2.1.1",
  "2.1.1-a.66.z.92",
  "1.0.0-x.7.z.92",
  "1.0.0-gamma+002",
  "1.0.0-beta+oof.sha.41af286"]

num :: [Char]
num = ['1'..'9']

parseNumStr :: Parser NumberOrString
parseNumStr =
  some alphaNum >>=
  \raw ->
    let final | getAll $ foldMap (All . (`elem` num)) raw  = return $ NOSI (read raw :: Integer)
              | otherwise = return $ NOSS raw
    in final

parseVer :: Parser SemVer
parseVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return $ SemVer major minor patch [] []

parseItem :: Parser NumberOrString
parseItem = do
  _ <- char '.'
  item <- parseNumStr
  return item

parseNumStrs :: Parser [NumberOrString]
parseNumStrs = do
  maj <- parseNumStr
  othr <- many parseItem
  return (maj:othr)

parseRelease :: Parser [NumberOrString]
parseRelease = do
  _ <- char '-'
  parseNumStrs

parseMeta :: Parser [NumberOrString]
parseMeta = do
  _ <- char '+'
  parseNumStrs

parseSemVer :: Parser SemVer
parseSemVer = do
  SemVer ma mi pa _ _ <- parseVer
  re <- option [] parseRelease
  meta <- option [] parseMeta
  return $ SemVer ma mi pa re meta

main :: IO ()
main = do
  let p par sample = print $ parseString par mempty sample
  traverse_ (p parseSemVer) testCases
