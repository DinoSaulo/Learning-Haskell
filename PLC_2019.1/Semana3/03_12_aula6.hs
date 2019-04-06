type Pessoa = String
type Livro = String
type BD = [(Pessoa, Livro, Int)]

livros2 ((x,y):xs) p
 | x == p = y : livros2 xs p
 | otherwise = livros2 xs p

tamLista :: [t] -> Int
tamLista []     = 0
tamLista (x:xs) = 1 + tamLista xs

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

inc :: Int -> Int
inc x = x + 1

vendas :: Int -> Int
vendas 0 = 4
vendas 1 = 0
vendas 2 = 8
vendas 3 = 5

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = f n + total f (n-1)

{-

total vendas 3
 = vendas 3 + total vendas 2
 = vendas 3 + vendas 2 + total vendas 1
 = vendas 3 + vendas 2 + vendas 1 + total vendas 0
 = vendas 3 + vendas 2 + vendas 1 + vendas 0
 = 5 + 8 + 0 + 4

-}

totalVendas n = total vendas n

sq :: Int -> Int
sq x = x * x

somaQuadrados n = total sq n

{-

somaQuadrados 2
= total sq 2
= sq 2 + total sq 1
= sq 2 + sq 1 + total sq 0
= sq 2 + sq 1 + sq 0
= 4 + 1 + 0
= 5

-}

maxi :: Int -> Int -> Int
maxi x y
 | x >= y    = x
 | otherwise = y

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = maxi (maxFun f (n-1)) (f n)

{-

maxfun sq 2
= maxi ( maxfun sq 1 ) ( sq 2 )
= maxi ( maxi (maxFun sq 0) ( sq 1 ) ) ( sq 2 )
= maxi ( maxi (sq 0) ( sq 1 ) ) ( sq 2 )
= maxi ( maxi 0 1 ) ( sq 2 )
= maxi 1 4
= 4

-}

zeroInRange :: (Int -> Int) -> Int -> Bool
zeroInRange f 0 = ( f 0 == 0)
zeroInRange f n = zeroInRange f (n-1) || (f n == 0)
{-

zeroInRange sq 2
= zeroInRange sq 1 || sq 2 == 0
= (zeroInRange sq 0 || sq 1) || sq 2 == 0
= ( sq 0 == 0  || sq 1 == 0) || sq 2 == 0
= (true || false) || false
= true

-}