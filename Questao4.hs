module Questao4 (Queue(..), enqueue, dequeue, peek, len, isEmpty, clear, contains, search) where

data Queue a = Queue [a]
    deriving (Eq, Show)

enqueue x (Queue xs) = Queue (xs ++ [x])

dequeue (Queue []) = (Nothing, Queue [])
dequeue (Queue (x:xs)) = (Just x, Queue xs)

peek (Queue []) = Nothing
peek (Queue xs) = Just (head xs)

len (Queue []) = 0
len (Queue xs) = 1 + len (Queue (tail xs))

isEmpty (Queue xs) = len (Queue xs) == 0

contains _ (Queue []) = False
contains n (Queue xs)
    | x /= n = contains n dequeuedQueue
    | x == Nothing = False
    | otherwise = True
    where
        (x, dequeuedQueue) = dequeue (Queue xs)

search n (Queue xs)
    | not (contains n (Queue xs)) = -1
    | n == x = 0
    | otherwise = 1 + search n dequeuedQueue
    where
        (x, dequeuedQueue) = dequeue (Queue xs)

clear (Queue xs) = (Queue [])
