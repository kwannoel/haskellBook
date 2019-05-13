module Exercises6 where

import Data.Foldable (fold)
import           Control.Applicative.Combinators (count', (<|>))
import           Data.Char                       (toLower)
import           Data.List                       (elemIndex)
import           Data.Monoid                     (Product (..))
import           Data.Word
import           Test.QuickCheck                 hiding (Result)
import           Text.Trifecta
import           Util                            (createNZeros, precedeZeros)

{-
Flow:
# Parse to basic form (List of Strings)
# Convert to intermediate form
# Validate a successful or failing parse
# Convert to Word64 x Word64

- Scenarios of collapse
- start, end, in between

-}
data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord)

data Seg = Sequence String | Collapsed deriving Show
-- maxBound for the above is 2 ^ 64 ^ 2 i.e. 2 ^ 128
-- taking 0000:0000:0000:0000:0000:0000:0000:0000
--         1    2    3    4    5    6    7    8
-- we have (16 ^ 4) ^ 8

instance Show IPAddress6 where
  show (IPAddress6 w1 w2) = go w1 [] 0 ++ ":" ++ go w2 [] 0 where
    go b64 result c = let (h, l) = divMod b64 (16 ^ c)
                          in case (h, l) of
                              (qu, 0) -> fold (result ++ [":" ++ show qu])
                              (qu, re) -> go qu (result ++ [":" ++ show re]) (c + 1)

-- globals

max64 :: Integer
max64 = toInteger (maxBound :: Word64)

-- Converters

nums :: String
nums = ['0'..'9']

chars :: String
chars = ['a'..'f']

intToIpv6 :: Integer -> IPAddress6
intToIpv6 x | x' <= max64
                    = IPAddress6 0 (fromInteger . toInteger $ x)
            | otherwise
                    = let (w1, w2) = divMod x' (max64 + 1)
                      in IPAddress6 (to64 w1) (to64 w2)
            where x' = toInteger x
                  max64 = toInteger (maxBound :: Word64)
                  to64 = fromInteger . toInteger

ipv6ToInt :: IPAddress6 -> Integer
ipv6ToInt (IPAddress6 w1 w2) = w1' * (mB + 1) + w2'
  where
    [w1', w2', mB] =
          fromInteger . toInteger
      <$> [w1, w2, maxBound :: Word64]

instance Arbitrary IPAddress6 where
  arbitrary = do
    w1 <- arbitrary
    w2 <- arbitrary
    return $ IPAddress6 w1 w2

unCollapse :: [Seg] -> String
unCollapse [] = []
unCollapse t@(x:xs) =
  case x of
    Collapsed    -> createNZeros (4 * (1 + 8 - length t)) ++ unCollapse xs
    Sequence str -> precedeZeros 4 str ++ unCollapse xs

hexConvert :: Char -> Integer
hexConvert c | c `elem` nums = read [c]
             | toLower c `elem` chars = let Just hex = elemIndex (toLower c) chars
                                        in toInteger hex + 10
             | otherwise = 0

segToInt :: [Seg] -> Integer
segToInt xs =
  go 0 0 (unCollapse xs)
  where
    go :: Integer -> Integer -> String -> Integer
    go counter result xs' = case xs' of
        [] -> result
        (a:as) -> go (counter + 1)
                      (hexConvert a * (m64 ^ (counter + 1)) + result)
                      as
    m64 :: Integer
    m64 = fromInteger . toInteger $ (maxBound :: Word64)

--  getProduct $ foldMap (Product . hexConvert) (unCollapse xs)

-- Parsers

-- Segm parses a non-empty segment delineated by :xxxx:
segm :: Parser Seg
segm = Sequence <$> count' 1 4 hexDigit

-- Helper for parsing collapsed segments
segc :: Parser Seg
segc = string ":" >> return Collapsed

-- Intermediate section form

-- Making the intermediate form from basic form

segParser :: Parser [Seg]
segParser = do
  -- Initiate :: / AAAA:
  start <- (segc <|> segm) <* segc
  case start of
    -- ::
    Collapsed -> go [Collapsed] 1 -- :: CONT
             <|> return [start]   -- :: END

    -- AAAA:
    Sequence _ ->
      (segc >> (go [start, Collapsed] 1 -- AAAA:: CONT
                <|> return [start, Collapsed])) -- AAAA:: END
       <|> go [start] 0 -- AAAA: CONT

    where
      go :: [Seg] -> Int -> Parser [Seg]
      go rResult rCollapsed -- REPEAT xxxx: / ::
                            -- ENDWITH : / AAAA
                  | length rResult < 0 = fail "Somehow length got a negative result"
                  | rCollapsed >= 2 = fail "More than one collapsed segment"
                  | length rResult >= 8 = fail "Address segment too long"
                  | otherwise =
                    -- repeated AAAA: / ::
                      try (do
                            sc <- (segm <|> segc) <* segc <?> "Control Flow"
                            case sc of
                              Collapsed  -> go (rResult ++ [sc]) (rCollapsed + 1)
                              Sequence _ -> go (rResult ++ [sc]) rCollapsed
                          )

                     -- : END
                      <|> (segc >> if rCollapsed < 1
                                   then return (rResult ++ [Collapsed])
                                   else fail "Too many Collapsed notations")

                      -- xxxx END
                      <|> (segm >>= \se'' ->
                              if length rResult == 8
                              || rCollapsed == 1 && length rResult <8
                              then return (rResult ++ [se''])
                              else fail "Incomplete IPV6 Address")

ipv6Parser :: Parser IPAddress6
ipv6Parser = intToIpv6 . segToInt <$> segParser

testCases :: [String]
testCases = ["0000::0000",
             "123A::1223:1233:1111:1111:1111",
             "0000:1231::",
             "::0100:1111",
             "::",
             "::1234:1111:1111:1111",
             "1234:1111:1233::",
             "1234::1234::1111::::2:::::",
             "AAAA::",
             "AAAA:AAAA::"]

fTestCases :: [String]
fTestCases = ["",
              "xyzd:1232:2111::11",
              "abcd::1234::aaaa",
             "1234:1111:1111:",
             "12da:1112:1233"
             ]

-- Props

main :: IO ()
main = do
  let hexMax :: Integer
      hexMax = 16
      p :: Parser a -> String -> Result a
      p pa = parseString pa mempty
  quickCheck (\c -> hexConvert c <= hexMax)
  quickCheck (\ipv6 -> (intToIpv6 . ipv6ToInt) ipv6 == ipv6)
  putStrLn "\n\nUnit tests for sequence conversion to string\n\n"
  print $ unCollapse [Sequence "aaa", Collapsed, Sequence "bbb"]
  putStrLn "\n\n\n Success Test Cases \n\n\n"
  foldMap (print . p ipv6Parser) testCases
  putStrLn "\n\n\n Failing Test Cases \n\n\n"
  foldMap (print . p ipv6Parser) fTestCases
