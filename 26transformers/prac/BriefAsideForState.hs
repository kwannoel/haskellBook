module BriefAsideForState where

newtype State s a = State { runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f s = State $ fmap (\(a, b) -> (f a, b)) (runState s)

instance Applicative (State s) where
  pure a = State $ ( , ) a <$> id
  fs <*> as = State $ \s -> let (ab, s1) = runState fs s
                                (a, s2) = runState as s1
                            in (ab a, s2)

instance Monad (State s) where
  return = pure
  as >>= f = State $ \s -> let (a1, s1) = runState as s
                               as2 = f a1
                               (a2, s2) = runState as2 s1
                           in (a2, s2)
  as >> bs = as >>= \_ -> bs -- Notice here state is still passed (s1)
