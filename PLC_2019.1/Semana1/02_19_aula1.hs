constante :: Int
constante = 3

maior :: Bool
maior = constante > 40

yes :: Bool
yes = True

funcaoComUmArgumento :: Int -> Bool
funcaoComUmArgumento x = x > 50

quadrado :: Int -> Int
quadrado x = x * x

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

-- Uma funcao com guardas
-- Estas guardas tem uma função similar ao IF
-- e o otherwise funciona como um else

{-
Essa função ira retornar o maior numero
-}

maxi :: Int -> Int -> Int
maxi x y 
 | x >= y = x  -- guarda (exp booleana) = expr
 | otherwise = y

{-
Essa função irá verificar se o um número é o dobro do utro 
 isDouble 6 3
-> True
 isDouble 3 6
 -> True
 isDouble 2 5
 -> False
-}

isDouble :: Int -> Int -> Bool
isDouble x y
 | x == 2 * y = True
 | y == 2 * x = True
 | otherwise = False