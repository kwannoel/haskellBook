module Exercises where

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dl = unDL dl []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ((++ [x]) . unDL xs)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append d1 d2 = DL (unDL d1 . unDL d2)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n - 1) ([n] ++ xs)
  -- go (n - 2) ([n-1] ++ ([n] ++ xs))
  -- go (n - 3) ([n-2] ++ ([n-1] ++ ([n] ++ xs)))

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n - 1)
          (singleton n `append` xs)
  -- DL ((n:) . (xs))

main :: IO ()
main = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDlist 123456
  ]

-- Concatenation vs composition
