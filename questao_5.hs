--  matrícula, primeiro nome, sobrenome, período de ingresso e CRA.

data Aluno =
    Aluno String String String String Float 
    deriving (Eq, Show)

getCRA (Aluno _ _ _ _ cra) = cra

mediaCRA xs = foldr (\alu acc -> acc + getCRA alu) 0 xs / fromIntegral (length xs)

groupByCRA [] = []
groupByCRA xs = [(getCRA curr, filter (\alu-> getCRA alu == currCra) xs)] ++ groupByCRA (filter (\alu-> getCRA alu /= currCra) (tail xs))
    where
        curr = head xs
        currCra = getCRA curr

assertResult testName actual expected
    | expected == actual = putStrLn $ " ++ Passed! " ++ testName
    | otherwise = putStrLn $ " -- Fail! "  ++ testName ++  " | expected " ++ show expected ++ " and received " ++ show actual

runTests = do
    let listaAlunos = [Aluno "mat1" "nome1" "sobre1" "2021.1" 3.0, Aluno "mat2" "nome2" "sobre2" "2020.1" 7.0, Aluno "mat3" "nome3" "sobre3" "2022.2" 3.0, Aluno "mat4" "nome4" "sobre4" "2021.1" 6.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 7.0]
    let listaAlunos2 = [Aluno "mat1" "nome1" "sobre1" "2021.1" 3.0, Aluno "mat2" "nome2" "sobre2" "2020.1" 7.0, Aluno "mat4" "nome4" "sobre4" "2021.1" 6.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 4.0]
    let groupByCRAExpected = [(3, [Aluno "mat1" "nome1" "sobre1" "2021.1" 3.0, Aluno "mat3" "nome3" "sobre3" "2022.2" 3.0]), (7, [Aluno "mat2" "nome2" "sobre2" "2020.1" 7.0, Aluno "mat5" "nome5" "sobre5" "2020.1" 7.0]), (6, [Aluno "mat4" "nome4" "sobre4" "2021.1" 6.0])]
    putStrLn "+---------------+"
    putStrLn "| Running tests |"
    putStrLn "+---------------+"

    assertResult "Test group by cra do aluno" (groupByCRA listaAlunos) groupByCRAExpected
    assertResult "Test calculo da media de cra dos alunos" (mediaCRA listaAlunos) 5.2      -- 26 / 5 == 6
    assertResult "Test calculo da media de cra numero inteiro" (mediaCRA listaAlunos2) 5.0 -- 20 / 4 == 5
