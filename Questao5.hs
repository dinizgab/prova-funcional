module Questao5 (Aluno(..), mediaCRA, groupByCRA) where

--  matrícula, primeiro nome, sobrenome, período de ingresso e CRA.
data Aluno =
    Aluno String String String String Float 
    deriving (Eq, Show)

getCRA (Aluno _ _ _ _ cra) = cra

mediaCRA [] = Nothing
mediaCRA xs = Just (foldr (\alu acc -> acc + getCRA alu) 0 xs / fromIntegral (length xs))

groupByCRA [] = []
groupByCRA xs = [(getCRA curr, filter (\alu-> getCRA alu == currCra) xs)] ++ groupByCRA (filter (\alu-> getCRA alu /= currCra) (tail xs))
    where
        curr = head xs
        currCra = getCRA curr
