module Questao1 (closestPossible) where

allPairs xs = [(x, y) | x <- xs, y <- xs]

calculateDistance _ [] = []
calculateDistance n ((a, b) : xs) = [abs((a + b) - n)] ++ calculateDistance n xs

closestPossible _ [] = Nothing 
closestPossible _ [x] = Just ((x, x))
closestPossible n xs = Just ((filter (\(a, b) -> minDistance == abs((a + b) - n)) possiblePairs) !! 0)
    where
        possiblePairs = allPairs xs
        minDistance = minimum (calculateDistance n possiblePairs)
