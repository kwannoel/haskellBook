module OurExceptions where

import Control.Exception

data EATD =
    NotEven Int
  | NotDivThree Int deriving (Eq, Show)

-- special syntax for automatically
-- deriving Exception

instance Exception EATD

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | odd i = throwIO (NotEven i)
  | otherwise = return i

catchNotDivThree :: IO Int
                 -> (EATD -> IO Int)
                 -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int
             -> (EATD -> IO Int)
             -> IO Int
catchNotEven = catch

{-
catches :: IO a -> [Handler a] -> IO a

data Handler a where
  Handler :: Exception e
          => (e -> IO a) -> Handler a
-}

catchBoth :: IO Int -> IO Int
catchBoth ioInt =
  catches ioInt
  [ Handler
      (\(NotEven _) -> return maxBound)
  , Handler
      (\(NotDivThree _) -> return minBound)
  ]

