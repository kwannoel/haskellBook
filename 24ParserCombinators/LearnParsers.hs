module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

-- type Parser a = String -> Maybe (a, String)

-- State s a = s -> (a, s)

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \s ->
--     case s of
--       (x:xs) -> if c == x
--                 then [(c, xs)]
--                 else []
--       _ -> []

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

eofOne :: Parser Char
eofOne = eof >> one

eofOneTwo :: Parser Char
eofOneTwo = eof >> oneTwo

p123 :: String -> IO ()
p123 str = print $ parseString (string str) mempty str

p123' :: String -> IO ()
p123' str = print $ parseString (string str) mempty "123"


main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "eofOne:"
  testParse eofOne
  pNL "eofOneTwo"
  testParse eofOneTwo
  pNL "p123"
  p123 "1"
  p123 "12"
  p123 "123"
  p123' "1"
  p123' "12"
  p123' "123"
