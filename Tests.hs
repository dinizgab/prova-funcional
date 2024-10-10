module Tests where

import Questao1 (closestPossible)
import Questao2 (floorAndCeil)
import Questao3 (Stack(..), stack, pop, isEmpty, contains, search, len, peek, clear)
import Questao4 (Queue(..), enqueue, dequeue, isEmpty, contains, search, len, peek, clear)
import Questao5 (Aluno(..), groupByCRA, mediaCRA)

assertResult testName actual expected
    | expected == actual = putStrLn $ " ++ Passed! " ++ testName
    | otherwise = putStrLn $ " -- Fail! "  ++ testName ++  " | expected " ++ show expected ++ " and received " ++ show actual

runTests = do
    putStrLn "+------------------------------+"
    putStrLn "| Running tests for question 1 |"
    putStrLn "+------------------------------+"

    assertResult "1st Test Case from the doc" (closestPossible 54 [10, 22, 28, 29, 30, 40]) (Just (22, 30))
    assertResult "2nd Test Case from the doc" (closestPossible 15 [1, 3, 4, 7, 10]) (Just (4, 10))
    assertResult "Testing with empty list" (closestPossible 15 []) Nothing 
    assertResult "Testing with list with just one element" (closestPossible 15 [1]) (Just (1, 1))
    assertResult "Testing with list with negative values search for positive" (closestPossible 15 [-3,-3,-2,-1]) (Just (-1, -1))
    assertResult "Testing with list with negative values searching for negative" (closestPossible (-6) [-3,-3,-2,-1]) (Just (-3, -3))
    assertResult "Testing with for tuple with sum above value" (closestPossible (7) [-5,-3,0,8]) (Just (0, 8))
    assertResult "Testing with for tuple subtracting its values" (closestPossible (7) [-5,-2,0,8]) (Just (-2, 8))
    assertResult "Testing with for tuple with sum above 0 with bothn negative values" (closestPossible (1) [-3,-1,8]) (Just (-1, -1))

    putStrLn "+------------------------------+"
    putStrLn "| Running tests for question 2 |"
    putStrLn "+------------------------------+"

    let orderedList = [1,2,4,5,6,8,9]
    let unorderedList = [3,6,1,9,0,4,10] --  0,1,3,4,6,9,10
    let negativeList = [-3,-4,2,3,0]     -- -4,-3,0,2,3
    let negativeList2 = [-3,-5,-1]       -- -5,-3,-1
    let emptyList = []

    assertResult "Testing with an empty List" (floorAndCeil 1 emptyList) (Nothing, Nothing)
    assertResult "Testing first value inside an ordered list" (floorAndCeil 1 orderedList) (Just 1, Just 1)
    assertResult "Testing last value inside an ordered list"(floorAndCeil 9 orderedList) (Just 9, Just 9)
    assertResult "Testing result below all values in an ordered list" (floorAndCeil 0 orderedList) (Nothing, Just 1)
    assertResult "Testing result below 0 and all values in an ordered list" (floorAndCeil (-1) orderedList) (Nothing, Just 1)
    assertResult "Testing result above all values in an ordered list" (floorAndCeil 10 orderedList) (Just 9, Nothing)
    assertResult "Testing value in the middle of an ordered list that exists in the list" (floorAndCeil 5 orderedList) (Just 5, Just 5)
    assertResult "Testing value in the middle of an ordered list that doesn't exist in the list" (floorAndCeil 3 orderedList) (Just 2, Just 4)
    assertResult "Testing value smaller than all the values from the list" (floorAndCeil (-1) unorderedList) (Nothing, Just 0)
    assertResult "Testing smallest value of the list" (floorAndCeil 0 unorderedList) (Just 0, Just 0)
    assertResult "Testing second smallest value" (floorAndCeil 1 unorderedList) (Just 1, Just 1)
    assertResult "Testing biggest value of the list" (floorAndCeil 10 unorderedList) (Just 10, Just 10)
    assertResult "Testint value bigger than all the values in the list" (floorAndCeil 11 unorderedList) (Just 10, Nothing)
    assertResult "Testing value inside an unordered list" (floorAndCeil 3 unorderedList) (Just 3, Just 3)
    assertResult "Testing value that doesn't exist in an unordered list" (floorAndCeil 2 unorderedList) (Just 1, Just 3)
    assertResult "Testing Value that doesn't exist with (ceil = value + 2)" (floorAndCeil 7 unorderedList) (Just 6, Just 9)
    assertResult "Testing value that doesn't exist with (floor = value - 2)" (floorAndCeil 8 unorderedList) (Just 6, Just 9)
    assertResult "Testing last value inside the list" (floorAndCeil 9 unorderedList) (Just 9, Just 9)
    assertResult "Testing value inside a list with negative and positive values" (floorAndCeil (-4) negativeList) (Just (-4), Just (-4))
    assertResult "Test value smaller than all the values in the list os negative and positive" (floorAndCeil (-5) negativeList) (Nothing, Just (-4))
    assertResult "Test value that isn't in the list of positive and negative" (floorAndCeil (-1) negativeList) (Just (-3), Just 0)
    assertResult "Testing value bigger then all the of the list" (floorAndCeil (4) negativeList)  (Just 3, Nothing)
    assertResult "Testing negative value in only negative value list" (floorAndCeil (-4) negativeList2) (Just (-5), Just (-3))

    putStrLn "+------------------------------+"
    putStrLn "| Running tests for question 3 |"
    putStrLn "+------------------------------+"

    let st :: Stack Int = Stack []
    let st1 = stack 1 st
    let st2 = stack 2 st1
    let st3 = stack 3 st2

    assertResult "Testing if stack is initialized with 0" (Questao3.len st) 0
    assertResult "Testing addition of item into a stack" (Questao3.peek st1) (Just 1)
    assertResult "Testing the length of the stack after adding an item" (Questao3.len st1) 1
    assertResult "Testing peek after adding another element" (Questao3.peek st2) (Just 2)
    assertResult "Testing adding more elements into the stack" (Questao3.len st2) 2
    assertResult "Testing popping an element" (Questao3.pop st2) (Just 2, st1)
    assertResult "Testing popping an element of an empty stack" (Questao3.pop st) (Nothing, st)
    assertResult "Testing peeking an empty stack" (Questao3.peek st) (Nothing)
    assertResult "Testing clear stack" (Questao3.len (Questao3.clear st2)) 0
    assertResult "Testing contains element in a stack" (Questao3.contains (Just 2) st2) True
    assertResult "Testing contains element that are not in the stack" (Questao3.contains (Just 3) st2) False
    assertResult "Testing contains element on an empty stack" (Questao3.contains (Just 3) st) False
    assertResult "Testing search element that are not in the stack" (Questao3.search (Just 3) st2) (-1)
    assertResult "Testing search element in the bottom of the stack" (Questao3.search (Just 1) st3) 2
    assertResult "Testing search element in the middle of the stack" (Questao3.search (Just 2) st3) 1
    assertResult "Testing search element in the top of the stack" (Questao3.search (Just 3) st3) 0
    assertResult "Testing search element on an empty stack" (Questao3.search (Just 1) st) (-1)
    assertResult "Testing is empty on an empty stack" (Questao3.isEmpty st) True
    assertResult "Testing is empty on a not empty stack" (Questao3.isEmpty st2) False

    putStrLn "+------------------------------+"
    putStrLn "| Running tests for question 4 |"
    putStrLn "+------------------------------+"

    let q :: Queue Int = Queue []
    let q1 = enqueue 1 q
    let q2 = enqueue 2 q1
    let q3 = enqueue 3 q2

    assertResult "Testing if queie is initialized with 0" (Questao4.len q) 0
    assertResult "Testing addition of item into a queue" (Questao4.peek q1) (Just 1)
    assertResult "Testing the length of the queue after adding an item" (Questao4.len q1) 1
    assertResult "Testing peek after adding another element" (Questao4.peek q2) (Just 1)
    assertResult "Testing peeking an empty queue" (Questao4.peek q) (Nothing)
    assertResult "Testing clear queue" (Questao4.len (Questao4.clear q2)) 0
    assertResult "Testing is empty on an empty queue" (Questao4.isEmpty q) True
    assertResult "Testing is empty on a not empty queue" (Questao4.isEmpty q2) False
    assertResult "Testing dequeuing an element from an empty queue" (Questao4.dequeue q) (Nothing, q)
    assertResult "Testing dequeuing an element from a queue" (Questao4.dequeue q1) (Just 1, q)
    assertResult "Testing dequeuing an element from a queue with more than one elements" (Questao4.dequeue q2) (Just 1, Queue [2])
    assertResult "Testing contains element in a queue" (Questao4.contains (Just 2) q2) True
    assertResult "Testing contains element that are not in the queue" (Questao4.contains (Just 3) q2) False
    assertResult "Testing contains element on an empty queue" (Questao4.contains (Just 3) q) False
    assertResult "Testing search element that are not in the queue" (Questao4.search (Just 3) q2) (-1)
    assertResult "Testing search element in the start of the queue" (Questao4.search (Just 1) q3) 0
    assertResult "Testing search element in the middle of the queue" (Questao4.search (Just 2) q3) 1
    assertResult "Testing search element in the end of the queue" (Questao4.search (Just 3) q3) 2
    assertResult "Testing search element on an empty queue" (Questao4.search (Just 1) q) (-1)

    putStrLn "+------------------------------+"
    putStrLn "| Running tests for question 5 |"
    putStrLn "+------------------------------+"

    let listaAlunos = [Aluno "mat1" "nome1" "sobre1" "2021.1" 3.0, Aluno "mat2" "nome2" "sobre2" "2020.1" 7.0, Aluno "mat3" "nome3" "sobre3" "2022.2" 3.0, Aluno "mat4" "nome4" "sobre4" "2021.1" 6.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 7.0]
    let listaAlunos2 = [Aluno "mat1" "nome1" "sobre1" "2021.1" 3.0, Aluno "mat2" "nome2" "sobre2" "2020.1" 7.0, Aluno "mat4" "nome4" "sobre4" "2021.1" 6.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 4.0]
    let groupByCRAExpected = [(3, [Aluno "mat1" "nome1" "sobre1" "2021.1" 3.0, Aluno "mat3" "nome3" "sobre3" "2022.2" 3.0]), (7, [Aluno "mat2" "nome2" "sobre2" "2020.1" 7.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 7.0]), (6, [Aluno "mat4" "nome4" "sobre4" "2021.1" 6.0])]
    let listaAlunosMaxValues = [Aluno "mat1" "nome1" "sobre1" "2021.1" 10.0, Aluno "mat2" "nome2" "sobre2" "2020.1" 10.0, Aluno "mat4" "nome4" "sobre4" "2021.1" 10.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 10.0]
    let listaAlunosMinValues = [Aluno "mat1" "nome1" "sobre1" "2021.1" 0.0, Aluno "mat2" "nome2" "sobre2" "2020.1" 0.0, Aluno "mat4" "nome4" "sobre4" "2021.1" 0.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 0.0]
    
    assertResult "Test group by student cra" (groupByCRA listaAlunos) groupByCRAExpected
    assertResult "Test group by student cra with empty list" (groupByCRA []) []
    assertResult "Test group by student cra with one student" (groupByCRA [Aluno "matx" "nomex" "sobrex" "2020.2" 0.0]) [(0.0, [Aluno "matx" "nomex" "sobrex" "2020.2" 0.0])]
    assertResult "Test student cra calculation" (mediaCRA listaAlunos) (Just 5.2)      -- 26 / 5 == 5.2
    assertResult "Test student cra mean calculation returning a round number" (mediaCRA listaAlunos2) (Just 5.0) -- 20 / 4 == 5
    assertResult "Test student cra mean calculation with all max values (10)" (mediaCRA listaAlunosMaxValues) (Just 10.0) -- 40 / 4 == 10
    assertResult "Test student cra mean calculation with all min values (0)" (mediaCRA listaAlunosMinValues) (Just 0.0) -- 0 / 4 == 0
    assertResult "Test student cra mean calculation with empty list" (mediaCRA []) Nothing

