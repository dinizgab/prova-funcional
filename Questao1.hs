module Questao1 (closestPossible) where

allPairs xs = [(x, y) | x <- xs, y <- xs, x /= y]

calculateDistance _ [] = []
calculateDistance n ((a, b) : xs) = [abs((a + b) - n)] ++ calculateDistance n xs

closestPossible _ [] = error "Empty List!"
closestPossible _ [x] = (x, x)
closestPossible n xs = (filter (\(a, b) -> minDistance == abs((a + b) - n)) possiblePairs) !! 0
    where
        possiblePairs = allPairs xs
        minDistance = minimum (calculateDistance n possiblePairs)


runTests = do

    
