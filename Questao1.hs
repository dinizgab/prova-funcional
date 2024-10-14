module Questao1 (closestPossible) where

removeDuplicates [] = []
removeDuplicates (x:xs) = [x] ++ removeDuplicates (filter (/= x) xs)

allPairs xs = removeDuplicates [(x, y) | x <- xs, y <- xs, x /= y || (x == y && length (filter (== x) xs) == 2)]

calculateDistance _ [] = []
calculateDistance n ((a, b) : xs) = [abs((a + b) - n)] ++ calculateDistance n xs

closestPossible _ [] = Nothing 
closestPossible _ [x] = Nothing
closestPossible n xs = Just ((filter (\(a, b) -> minDistance == abs((a + b) - n)) possiblePairs) !! 0)
    where
        possiblePairs = allPairs xs
        minDistance = minimum (calculateDistance n possiblePairs)
