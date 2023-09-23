module Setv2
    (Set,emptyS, addS,sizeS, belongs , removeS, unionS, setToList)
where

data Set a = Set [a]
{-
    INV.REP:
        *el conjunto Set contiene elementos repetidos
-}

emptyS :: Set a
--Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.

sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.

emptyS             = Set []
addS   x   (Set a) = Set (x : a)
sizeS      (Set a) = length a
belongs x  (Set a) = elem x a
removeS x  (Set a) = Set (eliminar x a)
unionS se1 (Set a) = unificar a se1 
setToList  (Set a) = sinRepetidos a

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x : sinRepetidos xs

unificar :: Eq a => [a] -> Set a -> Set a 
unificar []     con = con
unificar (x:xs) con = if belongs x con then unificar xs con else addS x (unificar xs con)


eliminar :: Eq a => a -> [a] -> [a]
eliminar x []  = []
eliminar x (y:ys) = if x == y then eliminar x ys else x : eliminar y ys