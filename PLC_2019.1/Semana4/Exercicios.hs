type Pessoa = String
type Livro = String
type BD = [(Pessoa, Livro)]

--livros :: BD -> Pessoa -> [Livro]

livros (x:xs) p
 | fst x = p = snd : livros xs p
 | otherwise = livros xs p 

livros2 ((x,y):xs) p 
 | x == p = y : livros xs p 
 | otherwise = livros xs p 

livros2 bd p = [snd p | x <- bd, fst x == p ]
livros3 bd p = [ y | (x,y) <- bd, x == p ]

-- polimorfismo (parametrico)

tamLista :: [t] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

-- Funcao de alta ordem

applyTwice :: (a -> a) -> a -> a
aplyTwice f x = f (f x)

inc :: Int -> Int
inc x = x + 1

--- 

