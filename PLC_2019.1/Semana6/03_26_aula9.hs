type Nome = String 
type ParInt = (Int, Int)

tamNome :: String -> Int
tamNome n = length n

somaPar :: ParInt -> Int
somaPar (x, y) = x + y

data ParTAlg = Par Int Int

somaParIntTAlg :: ParTAlg -> Int
somaParIntTAlg (Par x y) = x + y

-- Alterntivas

data Figura = Circulo Float | Retangulo Float Float

ehCircular :: Figura -> Bool
ehCircular(Circulo _) = True
ehCircular (Retangulo _ _) = False

area :: Figura -> Float
area (Circulo r) = pi * r * r
area (Retangulo l1 l2) = l1 * l2

-- Tipos recursivos

data Expr = Lit Int
            | Add Expr Expr
            | Sub Expr Expr

{-eval :: Expr -> Int
eval (Lit n)    = n
eval (Add e1 e2) = e1 + e2
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
-}
data ListaInt = Vazia | Cons Int ListaInt

somaL :: [Int] -> Int
somaL [x] = x
somaL (x:xs) = x + somaL xs

{-somaArvoreInt :: ArvoreInt -> Int
somaArvoreInt AVazia = 0
somaArvoreInt (No n aEsq aDir) = n + somaArvoreInt aEsq + somaArvoreInt aDir
-}

data Maybe a = Nothing | MJust a
mhead :: [t] -> Maybe t
mhead [] = MNothing
mhead (x:_) = MJust x
