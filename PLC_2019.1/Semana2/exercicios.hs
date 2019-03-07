-- dobrar os elementos de uma lista:

double :: [Int] -> [Int]
double [] = []
double (x:xs) = [x * 2] ++ double xs

double2 :: [Int] -> [Int]
double2 [] = []
double2 (x:xs) = (2*x) : double xs 

-- determinar se um valor faz parte de uma lista

member :: [Int] -> Int -> Bool
member []  x = False
member (x:xs) y
 | x == y = True
 | otherwise = member xs y

digitosLista :: String -> String
digitosLista [] = []
digitosLista (a:as)
 | ehDigito a = a : digitosLista as
 | otherwise = digitosLista as

ehDigito :: Char -> Bool
ehDigito ch = ('0' >= ch) && (ch <= '9')