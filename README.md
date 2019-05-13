# haskellBook
Haskell Book exercises from chapter 23 onwards.

# Helpful tips
To modify leading arguments, we can use lambda expressions.
i.e. a function f :: a -> b
we can do \x -> case someFunc x of
                  x1 -> f x
                  x2 -> f' x
