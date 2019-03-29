-- Classes
 {-
Utiliadas para falar de sobrecarga ou overload
Polimorfismo - Ah hoc       - Correção e Sobrecraga
             - Universal    - Parametrico e Inclusão
 -}
type Nome = String
data Pessoa = M Nome | F Nome
instance Pessoa eq where
    (M n1) == (M n2) = n1 == n2
    (F n1) == (F n2) = n1 == n2
    (M _) == (F _)   = False
    (F _) == (M _)   = False

class Visivel t where
    toString    :: t -> String
    size        :: t -> Int

instance Visivel Bool where
    toString True = "True"
    toString False = "False"
    size _ = 1

instance Visivel Char where
    toString c = [c]
    size _     = 1

instance Visivel Pessoa where
    toString (M n) = "A Pessoa: " ++ n ++ " "
    toString (F n) = "A pessoa: " ++ n  + " "
    size (M _) = 1
    size (F _) = 1

instance Visivel  => Visivel [a] where
    toString = concat . ( map toString )
    size     = (fold (+) 0) . ( map size )

data Temperatura = Quente | Frio deriving (Eq, Ord, Show)
data DiasSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab deriving (Eq, Ord, Show, Enum)
