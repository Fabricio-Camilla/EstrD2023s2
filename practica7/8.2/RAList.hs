module RAList
    (RAList,)
where

data RAList a = MkR Int (Map Int a) (Heap a)

emptyRAL :: RAList a
--Propósito: devuelve una lista vacía.
--Eficiencia: O(1).
isEmptyRAL :: RAList a -> Bool
--Propósito: indica si la lista está vacía.
--Eficiencia: O(1).
lengthRAL :: RAList a -> Int
--Propósito: devuelve la cantidad de elementos.
--Eficiencia: O(1).
get :: Int -> RAList a -> a
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
--Eficiencia: O(log N).
minRAL :: Ord a => RAList a -> a
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(1).
add :: Ord a => a -> RAList a -> RAList a
--Propósito: agrega un elemento al final de la lista.
--Eficiencia: O(log N).
elems :: Ord a => RAList a -> [a]
--Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
--Eficiencia: O(N log N).
remove :: Ord a => RAList a -> RAList a
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(N log N).
set :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
--Eficiencia: O(N log N).
addAt :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
--Eficiencia: O(N log N).
--Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
--también como argumento la máxima posición posible.
