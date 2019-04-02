-- Primeira questão

-- Não está correta, há um erro de sintáxe devido aos parâmetros

-- Segunda questão
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas [a] = [[a]]

-- Terceira questão
-- A
poli :: Int -> Int -> Int -> Int -> Int
poli a b c x = (a * (x ^ 2)) + (b * x) + c

{-
 a = 1; b = 2; c = 3; x = 4;
 1 * 4^2 + 2 * 4 + 3
 16 + 8 + 3
 27
-}
-- B
listaPoli :: [( Int, Int, Int)] −> [ Int −> Int ]


-- C
appListaPoli :: [Int −> Int] −> [Int] −> [Int]

