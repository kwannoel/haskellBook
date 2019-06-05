module SimpleQueue where

import Criterion.Main

-- Queue implementation:
-- push: prepend to enqueue
-- pop: drop from head of dequeue
-- for pop: whenever a dequeue is empty we reverse and clear the head of the list
-- getList: combine enqueue and dequeue

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a1 qa = Queue (a1 : enqueue qa) (dequeue qa)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue e d) = case Queue e d of
  Queue xs [] -> pop $ Queue [] $ reverse xs
  Queue xs (y:ys) -> Just (y, Queue xs ys)

pop' :: [a] -> Maybe (a, [a])
pop' l = go l []
  where go l1 l2 = case l1 of
          [] -> Nothing
          [x] -> Just (x, l2)
          x:xs -> go xs (l2 ++ [x])

getList :: Queue a -> [a]
getList q = enqueue q ++ reverse (dequeue q)

largeList :: [Int]
largeList = [1..10000]

pushPopQ :: Int -> Queue Int
pushPopQ i = go (Queue [] []) i
  where go q 0 = q
        go q i1 | even i1 = let Just (_, q1) = pop largeQueue
                            in go q1 (i1 - 1)
                | odd i1 = go (push 1 q) (i1 - 1)
                | otherwise = error "Unexpected"
               where largeQueue = Queue largeList []

pushPopL :: Int -> [Int]
pushPopL i = go [] i
  where go l 0 = l
        go l i1 | i1 == 0 = l
                | even i1 = let Just (_, l1) = pop' largeList
                            in go l1 (i1 - 1)
                | odd i1 = go (1 : l) (i1 - 1)
                | otherwise = error "unexpected"

main :: IO ()
main = defaultMain
  [ bench "push pop list" $
    whnf pushPopL 20000
  , bench "push pop queue" $
    whnf pushPopQ 20000
  ]
