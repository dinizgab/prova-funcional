module Questao3 (Stack(..), stack, pop, peek, len, isEmpty, clear, contains, search) where

data Stack a = Stack [a]
    deriving (Eq, Show)

-- A gente poderia inverter a ordem da pilha tambem
--
-- stack x (Stack xs) = [x] ++ xs
--
-- O acesso do pop seria bem mais simples com ela invertida,
-- ja que o topo da pilha estaria no primeiro elemento
--
-- pop (Stack xs) = (head xs, Stack (tail xs))

stack x (Stack xs) = Stack (xs ++ [x])

popAux [] = (Nothing, []) 
popAux [x] = (Just x, [])
popAux (x:xs) = (result, [x] ++ rest)
    where
        (result, rest) = popAux (xs)

pop (Stack xs) = (x, (Stack poppedStack))
    where
        (x, poppedStack) = popAux (xs)

peek (Stack []) = Nothing
peek (Stack [x]) = Just x
peek (Stack (x:xs)) = peek (Stack xs)

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
