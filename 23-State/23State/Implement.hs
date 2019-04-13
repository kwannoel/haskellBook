module Implement where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
--  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s1) = g s
                         in (f a, s1)

instance Applicative (Moi s) where
  pure a = Moi $ \s -> (a, s)
  -- <*> :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi a = Moi $ \s -> let (a1, s1) = a s
                                    (ab, s2) = f s1
                                in  (ab a1, s2)
