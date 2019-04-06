-- Funcoes de alta ordem 

aplicaDuasVezes :: (t -> t) -> t -> t
aplicaDuasVezes f x = f (f x)

inc :: Int -> Int
inc x = x + 1

quadrado :: Int -> Int
quadrado x = x * x

-- Funcoes de alta ordem para listas


-- Mapeamento

dobroL :: [Int] -> [Int]
dobroL [] = []
dobroL (x:xs) = (*) 2 x : dobroL xs

quadradoL :: [Int] -> [Int]
quadradoL [] = []
quadradoL (x:xs) = (*) x x : quadradoL xs

ehPar :: Int -> Bool
ehPar x = (mod x 2) == 0

ehParL :: [Int] -> [Bool]
ehParL [] = []
ehParL (x:xs) = ehPar x : ehParL xs

mapL :: (t -> t) -> [t] -> [t]
mapL f [] = []
mapL f (x:xs) = f x : mapL f xs

{-

mapL quadrado [1, 2]
= quadrado 1 : mapL quadrado [2]
= quadrado 1 : (quadrado 2 : mapL quadrado [])
= quadrado 1 : (quadrado 2 : quadrado [])
= quadrado 1 : (quadrado 2 : [])
= [1, 2]
-}


-- função polimorfica de alta ordem 

mapL_CL f l = [ f x | x <- l]

-- map length [[1..4], [1..6], [2..70]]

-- Fold = dobrando a lista na metade e na metade

somaL :: [Int] -> Int
somaL [] = 0
somaL (x:xs) = (+) x (somaL xs) 

disjL :: [Bool] -> Bool
disjL [] = False
disjL (x:xs) = (||) x (disjL xs)

mfold :: (t -> t -> t) -> [t] -> t
mfold f [x] = x
mfold f (x:xs)  = f x (mfold f xs)

{-
mfold2 :: (t -> t -> t) -> t -> [t] -> t
mfold2 f v [] = f []
mfold2 f v (x:xs) = f x (mfold2 f v xs)
-}
-- filtro

parL :: [Int] -> [Int]
parL [] = []
parL (x:xs)
 | ehPar x = x : parL xs
 | otherwise = parL xs

maior10 :: Int -> Bool
maior10 x = x > 10

{- 
maior10L :: [Int] -> [Int]
maior10L [] = []
maior10L (x:xs)
 | maior10L x >= x : maior10L xs
 | otherwise x = maior10L xs
-}

