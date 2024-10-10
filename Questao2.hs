module Questao2 (floorAndCeil) where

import Data.List

binSearch n l left right
    | left > right = left
    | l !! middle > n = binSearch n l left (middle - 1) 
    | otherwise = binSearch n l (middle + 1) right
    where 
        middle = (left + right) `div` 2

floorAndCeil _ [] = (Nothing, Nothing)
floorAndCeil n l
    | elem n l = (Just n, Just n)
    | inputIndex >= (length l) - 1 = (Just (last sortedList), Nothing)
    | inputIndex <= 0 = (Nothing, Just (head sortedList))
    | otherwise = (Just (sortedList !! (inputIndex - 1)), Just (sortedList !! inputIndex))
    where
        inputIndex = binSearch n sortedList 0 ((length l) - 1)
        sortedList = sort l


