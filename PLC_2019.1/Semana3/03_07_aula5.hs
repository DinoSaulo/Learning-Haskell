

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

tamLista :: [t] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

ehPar :: Int -> Bool
ehPar x = ( mod x 2 ) == 0

dobrarLista :: [Int] - > [Int]
dobrarLista l = [ 2*x | z <- l ]