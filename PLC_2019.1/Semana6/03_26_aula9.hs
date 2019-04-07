type Nome = String 
type ParInt = (Int, Int)

tamNome :: String -> Int
tamNome n = length n

somaPar :: ParInt -> Int
somaPar (x, y) = x + y

data ParTAlg = Par Int Int


data Temp = Frio | Quente deriving (Eq, Show, Ord)
data DiasSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab deriving (Show, Enum)
data Estacao = Inverno | Verao | Outono | Primavera


clima :: Estacao -> Temp
clima Inverno  = Frio
clima _        = Quente

somaParIntTAlg :: ParTAlg -> Int
somaParIntTAlg (Par x y) = x + y

-- Alterntivas

data Figura = Circulo Float | Retangulo Float Float deriving (Show)

circul = Circulo 3.5
quadr = Retangulo 3 3

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

express = Sub (Lit 5) (Lit 3)

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

data ListaInt = Vazia | Cons Int ListaInt

somaL :: [Int] -> Int
somaL [x] = x
somaL (x:xs) = x + somaL xs

{-somaArvoreInt :: ArvoreInt -> Int
somaArvoreInt AVazia = 0
somaArvoreInt (No n aEsq aDir) = n + somaArvoreInt aEsq + somaArvoreInt aDir

data maybe a = Nothing | MJust a
mhead :: [t] -> maybe t
mhead [] = MNothing
mhead (x:_) = MJust x
-}

---------------------------------


data ArvoreInt = AVazia | No Int ArvoreInt ArvoreInt deriving (Show)

arvre = No 10 ( No 5  ( No 5  (AVazia)  (AVazia))  ( No 5  (AVazia)  (AVazia))  ) ( No 11  (AVazia)  (AVazia)  )

somaArvoreInt :: ArvoreInt -> Int
somaArvoreInt  AVazia = 0
somaArvoreInt  (No n aEsq aDir) = n 
                                  + somaArvoreInt aEsq 
                                  + somaArvoreInt aDir
{-
*Main> somaArvoreInt AVazia
0
*Main> somaArvoreInt (No 1 (No 2 AVazia AVazia) AVazia)
3
-}

-- Tipos de dados polimorficos

-- data ListaPoli t = AVazia | Constr t (ListaPoli t) 

{-
*Main> :t Vazia
Vazia :: ListaPoli t
*Main> :t Constr 4 (Vazia)
Constr 4 (Vazia) :: Num t => ListaPoli t
*Main> :t Constr 'a' (Vazia)
Constr 'a' (Vazia) :: ListaPoli Char
*Main> :t Constr True (Vazia)
Constr True (Vazia) :: ListaPoli Bool
-}
data ArvorePoli t = ArvVazia | Node t (ArvorePoli t) (ArvorePoli t)
                     deriving (Show)

{-
*Main> :t Node 2 (ArvVazia) (ArvVazia )
Node 2 (ArvVazia) (ArvVazia ) :: Num t => ArvorePoli t
*Main> :t Node True (ArvVazia) (ArvVazia )
Node True (ArvVazia) (ArvVazia ) :: ArvorePoli Bool
*Main> :t Node 'a' (ArvVazia) (ArvVazia )
Node 'a' (ArvVazia) (ArvVazia ) :: ArvorePoli Char
-}

-- Tipo para uniao

data Either a b = Left a | Right b

{-
f :: -> Either Int Bool
f .... = Left x
         where x = expressao_Int
f .... = Right y
          where y = expressao_Bool
-}

data MMaybe a = MNothing | MJust a
-- Em Haskell, tipo Maybe
mhead :: [t] -> MMaybe t
mhead [] = MNothing
mhead (x:_) = MJust x

usandoMhead :: (Show t) => [t] -> String
usandoMhead l = case mhead l of
                 MNothing -> "Lista Vazia"
                 MJust x -> "Cabeca da lista: " ++ show x 