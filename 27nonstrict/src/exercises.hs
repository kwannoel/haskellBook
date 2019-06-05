module ExercisesEval where

-- 1
const 1 undefined = (\_ -> 1) undefined

-- 2
const undefined 1 = (\_ -> undefined) 1

-- 3
flip const undefined 1 = (\f a b -> f b a) const undefined 1
flip const undefined 1 = (\a b -> const b a) undefined 1
flip const undefined 1 = (\_ b -> b) undefined 1
flip const undefined 1 = (\b -> b) 1

-- sprint output
-- NF 1, 2, 4, 5
-- WHNF 3, 6

-- result in bottom?
-- Nope: 1, 4, 5, 6
-- Yep : 2, 3, 7

x = undefined
y = x `seq` "blah"
main = do
  print (snd (x, y))
