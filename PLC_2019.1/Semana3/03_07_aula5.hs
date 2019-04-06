member :: [Int] -> Int -> Bool
member [] y = False
member (x:xs) y 
 | x == y    = True
 | otherwise = member xs y

{-
maiorLista :: [Int] -> Int
maiorLista [] = minBound :: Int
maiorLista [x] = x
maiorLista (x:xs) =
 | x > maiorLista xs = x
 | otherwise = maiorLista xs

 primeiroDigito :: String  -> Char
 primeiroDigito st = case (digitosLista st) of
    [] -> '\0'
    (x:xs) -> x
-}
tamLista :: [t] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

{-

tamLista [3, 2, 1]
= 1 + tamLista [2, 1]
= 1 + 1 + tamLista [1]
= 1 + 1 + 1 + []
= 1 + 1 + 1 + 0
= 3

-}

digitosLista :: String -> String
digitosLista [] = []
digitosLista (a:as)
 | ehDigito a = a : digitosLista as
 | otherwise  = digitosLista as

ehPar :: Int -> Bool
ehPar x = ( mod x 2 ) == 0

ehDigito :: Char -> Bool
ehDigito ch =  (ch >= '0') && (ch <= '9')

primeiroDigito :: String -> Char
primeiroDigito st = case (digitosLista st) of
    [] -> '\0'
    (x:xs) -> x

-- Compreensao de lista

dobrarLista :: [Int] -> [Int]
dobrarLista l = [ 2*x | x <- l ]

dobrarListaPar :: [Int] -> [Int]
dobrarListaPar l = [ 2*x | x <- l, ehPar x ]

somarPares :: [(Int,Int)] -> [Int]
somarPares l = [ x + y | (x,y) <- l ]
-- somarPares l = [fst p + snd p, p <- l]

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort[y| y <- xs, y <= x] ++ 
               [x] ++ 
               qSort[ y | y <- xs, y > x]