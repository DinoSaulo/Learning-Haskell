maiorDeTresInteiros :: Int -> Int -> Int -> Int
maiorDeTresInteiros x y z
 | x >= y && y >= z = x -- se x é maior ou igual a y, e y é maior ou igula a z, logo, x é o maior
 | y >= z = y -- se y é maior ou igual a z ele será o maior
 | otherwise = z -- caso contrário z é o maior de todos

-- fromEnum serve para converter um caractere para o seu decimal equivalente na tabela ascII
-- e toEnum faz o inverso
-- Esta funcao soh faz 67 - 97
offSet :: Int
offSet = fromEnum 'A' - fromEnum 'a'

-- Ira retornar um caractere em maiusculo
toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offSet) -- o caractere convertido para o decimal + o offset depois convertidos para o caractere resultam no caractere maiusculo

-- Verifica se é um digito
-- isDigit '1'
-- -> True
-- isDigit 'Ben 10'
-- -> False
isDigit :: Char -> Bool
isDigit ch = (ch >= '0') && (ch <= '9')

-- Funcao para somar dois inteiros de uma tupla
addPair :: (Int, Int) -> Int
addPair (x,y) = x + y

-- Funcao para retornar o primeiro de uma tripla
primeiroElemTripla :: (Int, Int, Int) -> Int
primeiroElemTripla (x,y,z) = x

-- Calculo de raiz
umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = - b / (2.0 * a)

-- Calculo de raizes
duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = ((-b /(2 * a)) - (sqrt (b^2 - 4.0 * a * c) / (2.0 * a)) , (-b /(2* a)) + (sqrt (b^2 - 4.0 * a * c) / (2.0 * a)))

-- Cauculo de raizes com guardas
-- fst serve para pegar o primeiro numero de uma tupla e fst o segundo
raizes :: Float -> Float -> Float -> String
raizes a b c 
 | b^2 == 4.0 * a * c = show (umaRaiz a b c)
 | b^2 > 4.0 * a * c  =  show (fst (duasRaizes a b c))
                         ++ " "
                         ++ show (snd (duasRaizes a b c))
 | otherwise = "nao hah raizes"

vendas :: Int -> Int
vendas 0 = 5
vendas 1 = 12
vendas 2 = 3
vendas 3 = 9
vendas 4 = 8

-- Faz meio que uma fatorial
totalVendas :: Int -> Int
totalVendas n 
 | n == 0 = vendas 0       -- Caso base
 | n > 0 =  totalVendas (n-1) + vendas n  -- Caso recursivo

 {--
totalVendas 3 
= totalVendas 2 + vendas 3
= totalVendas 1 + vendas 2 + vendas 3
= totalVendas 0 + vendas 1 + vendas 2 + vendas 3
= vendas 0 + vendas 1 + vendas 2 + vendas 3
--}

-- maior de dois inteiros
maxi :: Int -> Int -> Int
maxi x y 
 | x >= y = x 
 | otherwise = y

maximoVendas :: Int -> Int 
maximoVendas n 
 | n == 0 = vendas 0
 | otherwise = maxi (maximoVendas (n-1)) (vendas n)

{--
maximoVendas 3 
= maxi (maximoVendas (2)) (vendas 3)
= maxi (maxi maximoVendas (1) vendas 2) (vendas 3)
= maxi (maxi (maximoVendas (0) vendas 1) vendas 2) (vendas 3)
= maxi (maxi (vendas (0) vendas 1) vendas 2) (vendas 3)
 --}

quadrado :: Int -> Int
quadrado x = x * x

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

maxi2 :: Int -> Int -> Int
maxi2 x y
 | x >= y = x      -- guarda (exp booleana) = expr 
 | otherwise = y 