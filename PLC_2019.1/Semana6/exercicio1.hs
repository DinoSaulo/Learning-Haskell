-- ShowExpr

data Expr = Lit Int
            | Add Expr Expr
            | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"

data Lista t = Vazia | Cons t (Lista t)

toList :: Lista t -> [t]
toList (Vazia) = []
toLista (Cons x l) = x : toList l

fromList :: [t] -> Lista t
fromList [] = Vazia
fromList (x:xs) = (Cons x (fromList xs))

-- criar uma arvore vazia
data NTree = NilT | Node Int NTree NTree

somaArv :: NTree -> Int
somaArv (NilT)      = 0
somaArv (Node x arvEsq arvDir) = x + somaArv arvEsq + somaArv arvDir
{-
Execução da soma da arvre
*Main> somaArv (Node 4 NilT NilT)
4
*Main> somaArv (Node 4 (Node 5 NilT NilT) NilT)
9
*Main> somaArv (Node 4 (Node 5 NilT NilT) (Node 3 NilT NilT))
-}

-- Profundidade da arvore

profundidadeDoTeuCU :: NTree -> Int
profundidadeDoTeuCU (NilT)  = 0
profundidadeDoTeuCU (Node _ arvEsq arvDir) = 1 + max (profundidadeDoTeuCU arvEsq) (profundidadeDoTeuCU arvDir)

collapse :: NTree -> Int
collapse (NilT)     = []
collapse (Node x t1 t2) = collapse t1 ++ [x] ++ collapse t2