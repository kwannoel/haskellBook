module IORefTrans where

import Control.Monad (replicateM)
import System.Random (randomRIO)

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True =
  replicateM 10 (randomRIO (0,10))
gimmeShelter False = pure [0]

-- The idea here is that while different literal values are produced when arg is true,
-- The result (a list of random numbers) is produced for the input

-- Referential transparency is preserved because we are producing the same IO action
