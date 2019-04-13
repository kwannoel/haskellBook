module Examples where

import System.Random
-- Random & State are similar

random :: (Random a) => StdGen -> (a, StdGen)
random = undefined

randomR :: {- (...) => -} (a, a) -> g -> (a, g)
randomR = undefined

-- State { runState :: s -> (a, s)}

-- This means random can be modelled as a state monad
