--  matrícula, primeiro nome, sobrenome, período de ingresso e CRA.

data Aluno mat nome sobrenome periodo cra =
    Aluno mat nome sobrenome periodo cra
    deriving (Eq, Show)

getCRA (Aluno _ _ _ _ cra) = cra

mediaCRA xs = fromIntegral (foldr (\alu acc -> acc + getCRA alu) 0 xs) / fromIntegral (length xs)

groupByCRA [] = []
groupByCRA xs = [(getCRA curr, filter (\alu-> getCRA alu == currCra) xs)] ++ groupByCRA (filter (\alu-> getCRA alu /= currCra) (tail xs))
    where
        curr = head xs
        currCra = getCRA curr
