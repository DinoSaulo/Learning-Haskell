fatorial :: Int -> Int
fatorial 0 = 1 
fatorial n = n * fatorial (n-1)

-- Listas

somaLista :: [Int] -> Int
somaLista [] = 0 -- caso Base
somaLista (x:xs) = x + somaLista xs -- caso recursivo

tamLista :: [Int] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

-- Função de concatenação de listas : ++
reverterLista :: [Int] -> [Int]
reverterLista [] = []
reverterLista (x:xs) = reverterLista xs ++ [x]

repeticao :: Int -> Char -> String
repeticao 0 ch = []
repeticao n ch = [ch] ++ repeticao (n-1) ch

mtake _ [] = []
mtake 0 _ = []
mtake n (x:xs) = x : mtake (n-1) xs


-- ordenacao

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y : ins x ys


-- dobrar os elementos de uma lista:

double :: [Int] -> [Int]
double [] = []
double (x:xs) = [x * 2] ++ double xs

-- determinar se um valor faz parte de uma lista

member :: [Int] -> Int -> Bool
