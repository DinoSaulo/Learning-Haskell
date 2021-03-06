--Dale

--QUEST�ES DOS SLIDES (N�O SEI SE � EXATAMENTE ASSIM QUE ERA PRA FAZER)

--LISTA 1

--Questao 1: est�  correto.
--map :: (a->b) -> [a] -> [b]
--(.) :: (c->d) -> (e->c) -> (e->d)
--map . (.)
--para map queremos saber quem � 'a' e quem � 'b'
--vamos usar (c->d) como i e ((e->c) -> (e->d)) como j
--dessa forma, nossa partida inicial seria i, (c->d).
--logo, para map (e->c) seria a e (e->d) seria b
--logo, o resultado seria (c->d) -> [e->c] -> [e->d]
--map . (.) = (c->d) -> [e->c] -> [e->d].


--Questao 2
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [x:ys | ys <- sublistas xs] ++ sublistas xs

--Questao 3
--a
poli :: Int -> Int -> Int -> Int -> Int
poli a b c = (\x -> a*(x*x) + b*x + c )

--b
listaPoli :: [(Int, Int, Int)] -> [Int->Int]
listaPoli l = [poli a b c | (a,b,c) <- l]--USANDO COMPREENS�O DE LISTA

listaPoliNormal :: [(Int, Int, Int)] -> [Int->Int]
listaPoliNormal [] = []
listaPoliNormal ((a,b,c):xs) = (poli a b c) : listaPoli xs--FAZENDO NORMAL

--c
appListaPoli :: [Int->Int] -> [Int] -> [Int]
appListaPoli lp lint = [p i | p <- lp, i <- lint]--USANDO COMPREENS�O DE LISTA

appListaPoliNormal :: [Int->Int] -> [Int] -> [Int]
appListaPoliNormal [] _ = []
appListaPoliNormal _ [] = []
appListaPoliNormal (x:xs) (a:b) = (x a):(appListaPoliNormal xs b)--FAZENDO NORMAL 

------------------------- Questao 4 ------------------------
-------------------- Questao 4, letra A --------------------

-- Percorre toda a lista e cria uma nova
-- A nova lista em o mesmo n�mero de elementos que a recebida
-- Por�m cada elemento � o n�mero 1
-- Ent�o se aplica a fun��o 'sum' � o n�mero retornado � o tamanho da lista
--
functionSizeList list = sum [1 | _ <- list]

-- Compara a linha atual da matrix, x, com a seguinde, (head xs)
-- Se o tamanho for o mesmo ent�o compara a seguinde com a depois desta e assim sucessivamente
-- Se todas tiverem o mesmo tamanho ent�o True � retornado ao chegar no caso "isMatrix [x] = True"
-- Se forem diferentes ent�o entra no "otherwise = False"
--
isMatrix :: [[t]] -> Bool
isMatrix [[]] = True
isMatrix [x] = True
isMatrix (x:xs)
| ((functionSizeList x) == (functionSizeList (head xs))) = isMatrix xs
| otherwise = False

-- Uma outra forma muito menos eficiente
-- Cria uma lista com o tamanho de todos os arrays presentes na matriz
-- verifica se todos s�o iguais
tamLista :: [t] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

lengthSubList :: [[t]] -> [Int]
lengthSubList [[]] = [0]
lengthSubList [x] = [tamLista x]
lengthSubList (x:xs) = [tamLista x] ++ lengthSubList xs

allEqualList :: [Int] -> Bool
allEqualList [] = True
allEqualList [x] = True
allEqualList (x:xs) = (x == (head xs)) && allEqualList xs

lengthMatrix :: [[t]] -> Bool
lengthMatrix [[]] = True
lengthMatrix [x] = True
lengthMatrix (x) = allEqualList (lengthSubList x)

-------------------- Questao 4, letra B --------------------

-- Percorre todas as linhas da matrix e retorna a linha "n"
-- Isso � feito chamando a fun��o removendo a 1 linha da matrix recebida a cada chamada da fun��o
-- Quando a fun��o for chamada "n" vezes "n" ser� "0" e ela entra no caso "otherwise = x", retornando a linha
--
functionGetLineMatrix :: [[t]] -> Int -> [t]
functionGetLineMatrix (x:xs) n
| (n /= 0) = functionGetLineMatrix xs (n-1)
| otherwise = x

-- Mesmo esquema da fun��o "functionGetLineMatrix"
-- Ao chegar na linha n ele substitui o que estava l� pela linha "l"
--
functionSwitchLineMatrix :: [[t]] -> [t] -> Int -> [[t]]
functionSwitchLineMatrix (x:xs) l n
| (n /= 0) = [x]++(functionSwitchLineMatrix xs l (n-1))
| otherwise = [l]++xs

-- Basicamente pegamos a linha "x" e colocamos na linha "y"
--                          [----------------Isso � feito nessa parte----------------]
-- functionSwitchLineMatrix (functionSwitchLineMatrix l (functionGetLineMatrix l x) y) (functionGetLineMatrix l y) x
-- Isso retorna uma matrix, ai chamamos a fun��o novamente passando essa matrix e a linha "y" para ser colocada na linha "x"
-- Por�m note que a linha da posi��o 'y' j� foi modificada nessa nova matrix, por isso pegamos a linha "y" da matrix original que nos foi dada, a "l"
--
-- Obs.: Usamos a matrix original apenas para pegar a linha "y" na segunda etapa, por�m passamos a j� modificada para a fun��o de troca, "functionSwitchLineMatrix"
--
switchLineMatrix :: [[t]] -> Int -> Int -> [[t]]
switchLineMatrix l x y = functionSwitchLineMatrix (functionSwitchLineMatrix l (functionGetLineMatrix l x) y) (functionGetLineMatrix l y) x


-------------------- Questao 5 --------------------

-- Retorna todas as sublistas cuja soma dos elementos impares � maior que a dos pares
--
filtrarList :: [[Int]] -> [[Int]]
filtrarList [[]] = [[]]
filtrarList (x:xs)
| ((sum (myFilter 1 x)) > (sum (myFilter 0 x))) && ((xs == [[]]) || null xs) = [x]
| ((sum (myFilter 1 x)) > (sum (myFilter 0 x))) == False && ((xs == [[]]) || null xs) = filtrarList [[]]
| ((sum (myFilter 1 x)) > (sum (myFilter 0 x))) = [x] ++ (filtrarList xs)
| otherwise = (filtrarList xs)
where
   myFilter i l = [a | a <- l, ((mod a 2) == i)]

-- Filtra as listas e
--      Filtra as lista novamente:                                              filtrarList x
--      Soma todos os elementos de cada lista j� filtrada:                      map sum (filtrarList x)
--      Pega o valor maximo da lista retornada e multiplica pelo valor "i":     (maximum (map sum (filtrarList x))) * i
--
filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int)
filtrarEInserir x i = (filtrarList x, (maximum (map sum (filtrarList x))) * i)

--Questao 6
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:[]) = [(f x)]
altMap f g (x:xs:[]) = [(f x)] ++ [(g xs)]
altMap f g (x:xs:xt) = [f x] ++ [(g xs)] ++ altMap f g xt



-------------------- Questao 6, outro jeito de fazer --------------------

-- Recebe duas fun��es e aplica a primeira sempre ao elemento da cabe�a da lista
-- Troca a 1 pela 2 ao chamar a fun��o de forma recursiva
-- Isso � uma gambiarra sem vergonha pra resolver o problema... ( * � v ` )_?
--
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = [f1 x] ++ (altMap f2 f1 xs)



-------------------- Questao 7, letra A --------------------

type Codigo = Int
data Voto = Presidente Codigo | Senador Codigo | Deputado Codigo | Branco deriving (Show)

type Urna = [Voto]
type Apuracao = [(Voto, Int)]

-- Definindo as instancias validas
--
instance Eq Voto where
   (Presidente p1) == (Presidente p2) = (p1 == p2)
   (Senador s1) == (Senador s2) = (s1 == s2)
   (Deputado d1) == (Deputado d2) = (d1 == d2)
   Branco == Branco = True
   _ == _ = False

-------------------- Questao 7, letra B --------------------

-- Percorre toda a lista e cria uma nova
-- A nova lista em o mesmo n�mero de elementos que a recebida
-- Por�m cada elemento � o n�mero 1
-- Ent�o se aplica a fun��o 'sum' � o n�mero retornado � o tamanho da lista
--
totalVotos :: Urna -> Voto -> Int
totalVotos urna voto = sum [1 | x <- urna, x == voto]

-------------------- Questao 7, letra C --------------------

-- Calcula o total de votos recebidos pra um candidado
-- Remove todas as entradas desse candidado da urna
-- Chama a fun��o novamente para calcular a apura��o do pr�ximo candidato
--
apurar :: Urna -> Apuracao
apurar [] = []
apurar lista@(x:xs) = [(x, totalVotos lista x)] ++ (apurar (functionRemoveAll lista x))
   where
       functionRemoveAll urna voto = [x | x <- urna, x /= voto]


--FINAL LISTA 1



-------------------------------- LISTA 2 ------------------------------
-------------------- Questao 1, letra A --------------------

f :: [Int] -> [Int]
f [] = []
f [x] = []
f (:xs)
| (x == (head xs)) && (null xs) = [x]
| (x == (head xs)) = x:(f xs)
| otherwise = f xs

-------------------- Questao 1, letra B --------------------

-- Criamos uma nova lista com a fun��o "zip" onde juntamos o "init" da lista original com o "tail" da lista original
-- Criando uma lista de tuplas, onde
--      o elemento "x" de cada tupla vem do "init" da lista original
--      o elemento "y" de cada tupla vem do "tail" da lista original
--
-- OBS.: Init se refere a toda a lista, exceto o �ltimo elemento
--       O tail � a cauda da lista, o equivalente ao 'ls' na quest�o
--
-- OBS2.: O zip funciona pegando os primeiros elementos de cada lista e colocando em uma tupla, depois os segundos elementos e etc
-- Se uma lista for maior que a outra, os elementos que sobrarem S�O DESCARTADOS
--      Isso acontece na quest�o, por isso usei o termo "init" para me referir a primeira lista, j� que o �ltimo elemento vai ser descartado da 1 lista
--
fCL (l:ls) = [x | (x,y) <- (zip (l:ls) ls), x == y]

-------------------- Curiosidades sobre as fun��es --------------------

-- A fun��o "zip" pega os elementos de duas listas em cria uma nova lista com estes elementos em uma tupla
-- Exemplo:
--      listaA = [1,2,3,4,5,6,7] = [1..7]
--      listaB = ['A', 'B', 'C', 'D', 'E'] = ['A'..'E']
--
--      zip listaA listaB = [(1,'A'), (2,'B'), (3,'C'), (4,'D'), (5,'E')]
--
-- Os elementos que sobram s�o descartados, j� que n�o tem como forma uma tupla
--
___________________________________________________________
-------------------- Questao 2 --------------------

-- � aplicada inicialmente uma fun��o de filtro para filtrar apenas os elementos maiores ou iguais a "0"
-- Da lista resultante se filtra todos os elementos menores ou iguais a 100
-- Ent�o se aplica a todos os elementos da lista a fun��o "even" (Se par = True, sen�o = False)
-- Com isso nos temos uma lista de booleanos
-- Ao aplicar "foldr" passando "True" como argumento com a fun��o de igualdade "=="
--      Retorna "True" se todos os argumentos forem "True"
--      Retorna "False" caso algum deles seja "False"
--
g :: [Int] -> Bool
g [] = True
g l = foldr (==) True (map even (filter (<= 100) (filter (>=0) l)))

-------------------- Curiosidades sobre as fun��es --------------------

-- A fun��o filter:
-- Aplica uma fun��o a uma LISTA como um todo e seleciona os ELEMENTOS que satisfazem a CONDI��O
--
-- filter :: (a -> Bool) -> [a] -> [a]
--

-- A fun��o map:
-- Aplica uma fun��o a CADA ELEMENTO de uma lista e retorna uma lista apos a aplica��o da fun��o em cada elemento
--
-- map :: (a -> b) -> [a] -> [b]que 
--

-- A fun��o foldr:
-- Aplica uma fun��o e um argumento ao �LTIMO ELEMENTO de uma lista
-- O resultado � passado como se fosse o novo argumento e a fun��o � aplicada ao PENULTIMO ELEMENTO da lista e assim sucessivamente
--
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--
___________________________________________________________
-------------------- Questao 3, letra A --------------------

type Fabricante = String
type Potencia = Int

data Lampada = Incandescente Fabricante Potencia | Compacta Fabricante Potencia

-- Usando as defini��es de instacias padr�o (N�o � isso que o prof. quer):
-- data Lampada = Incandescente Fabricante Potencia | Compacta Fabricante Potencia deriving (Eq, Show)

-------------------- Questao 3, letra B --------------------

-- Definindo as instancias para "Show"
instance Show Lampada where
   show (Incandescente f1 p1) = "Incandescente " ++ f1 ++ " " ++ show p1 ++ " Watts"
   show (Compacta f1 p1) = "Compacta " ++ f1 ++ " " ++ show p1 ++ " Watts"

-------------------- Questao 3, letra C --------------------

-- Definindo as instancias para "Eq"
--
instance Eq Lampada where
   (Compacta f1 p1) == (Compacta f2 p2) = (f1 == f2) && (p1 == p2)
   (Incandescente f1 p1) == (Incandescente f2 p2) = (f1 == f2) && (p1 == p2)
   _ == _ = False
