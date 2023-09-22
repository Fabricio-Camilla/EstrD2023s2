module Set
    (Set,emptyS, addS)--, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = Set [a]
{-
    INV.REP:

-}

emptyS :: Set a
--Crea un conjunto vacío.
addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
{-belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.-}

emptyS =Set []
addS x (Set a) = Set (x : a)

