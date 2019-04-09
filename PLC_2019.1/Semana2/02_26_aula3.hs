{- terceiro :: (Int, Char, Bool) -> Bool
terceiro (x,y,z) = z -}

terceiro :: (a, b, c) -> c
terceiro (x,y,z) = z

-- Sinonimos de tipos
type ParInt = (Int, Int)

primeiroParInt :: ParInt-> Int
primeiroParInt p = fst p

type Nome = String
type Idade = Int
type Pessoa = (Nome, Idade)
type ListaPessoa = [Pessoa]

nome :: Pessoa -> Nome
nome pessoa = fst pessoa

{-
nome ("Saulo", 20)  
-> "Saulo"
-}

type ParPoli t =  (t,t)
type ListaParPoli t = [ParPoli t]

-- Casamento de padrao

myNot :: Bool -> Bool
myNot False = True
myNot True = False

myOr1 :: Bool -> Bool -> Bool
myOr1 True True = True
myOr1 True False = True
myOr1 False True = True
myOr1 False False = False

myOr2 :: Bool -> Bool -> Bool
myOr2 True x = True
myOr2 False x = x

myOr3 :: Bool -> Bool -> Bool
myOr3 True _ = True
myOr3 False x = x

-- Fazendo a Or com guardas
myOr4 :: Bool -> Bool -> Bool
myOr4 x y
 |(x == False) && (y== False) = False
 |otherwise = True

-- And
myAnd1 :: Bool -> Bool -> Bool
myAnd1 True True = True
myAnd1 True False = False
myAnd1 False True = False
myAnd1 False False = False

-- ERRO: myAnd2 True False
myAnd2 :: Bool -> Bool -> Bool
myAnd2 True True = True
myAnd2 False _ = False

-- Fazendo a And com guardas
myAnd3 :: Bool -> Bool -> Bool
myAnd3 x y
 |(x == True) && (y== True) = True
 |otherwise = False

fatorial :: Int -> Int
fatorial x
 | x == 0    = 1                      -- caso base
 | otherwise =  x * fatorial (x-1)    -- caso recursivo

fatorialCasamPadrao :: Int -> Int
fatorialCasamPadrao 0 = 1
fatorialCasamPadrao n = n * fatorialCasamPadrao (n-1)

somaQuadrados :: Int -> Int -> Int -> Int
somaQuadrados x y z = sqX + sqY + sqZ
  where sqX = x * x
        sqY = y * y
        sqZ = z * z

somaQuadrados2 :: Int -> Int -> Int
somaQuadrados2 x y = sq x + sq y
    where sq n = n * n

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (p - t ,  p + t)
    where
        p = (-b /(2* a))
        t = (sqrt (b^2 - 4.0 * a * c) / (2.0 * a))

-- Calculo de raiz
umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = - b / (2.0 * a)

raizes :: Float -> Float -> Float -> String
raizes a b c 
    | b^2 == 4.0 * a * c = show (umaRaiz a b c)
    | b^2 > 4.0 * a * c  =  show (fst (duasRaizes a b c))
                            ++ " "
                            ++ show (snd (duasRaizes a b c))
    | otherwise = "nao hah raizes"