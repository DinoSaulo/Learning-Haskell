-- dobrar os elementos de uma lista:

double :: [Int] -> [Int]
double [] = []
double (x:xs) = [x * 2] ++ double xs

-- determinar se um valor faz parte de uma lista

member :: [Int] -> Int -> Bool