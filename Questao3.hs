module Questao3 (Stack(..), stack, pop, peek, len, isEmpty, clear, contains, search) where

data Stack a = Stack [a]
    deriving (Eq, Show)

stack x (Stack xs) = Stack (x : xs)

pop (Stack []) = (Nothing, Stack([])) 
pop (Stack (x:xs)) = (Just x, Stack (xs))

peek (Stack []) = Nothing
peek (Stack (x:xs)) = Just x

len (Stack []) = 0
len (Stack xs) = 1 + len (Stack (tail xs))

isEmpty xs = len xs == 0

clear (Stack xs) = (Stack [])

contains _ (Stack []) = False 
contains n (Stack xs)
    | x == Nothing = False
    | x /= n = contains n poppedStack
    | otherwise = True
    where
        (x, poppedStack) = pop (Stack xs)

-- Return the depth of the first time an element appears in a stack
-- Return -1 if the element isn't present
search n (Stack xs)
    | not (contains n (Stack xs)) = -1
    | n == x = 0
    | otherwise = 1 + search n poppedStack
    where
        (x, poppedStack) = pop (Stack xs)
