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