module RAList
    (RAList,)
where

data RAList a = MkR Int (Map Int a) (Heap a)

{-  MkR  i Mixa Ha
    INV REP:
        * El Int i representa la siguiente posicion del elemento a aregar en la lista y,
            debe ser mayor o igual a 0.
        * Cuando la lista se encuentra vacia su i es igual a 0.
        * En Mixa para cada clave Int p, tiene como valor un Elemento a.
        * Cada Elemento a valor en Mixa, su clave es igual a p. 
        * En Ha se encuentran contenidos todos los valores de Mixa.
-}

emptyRAL :: RAList a
--Propósito: devuelve una lista vacía.
--Eficiencia: O(1).
emptyRAL = MkR 0 emptyM emtpyH

isEmptyRAL :: RAList a -> Bool
--Propósito: indica si la lista está vacía.
--Eficiencia: O(1).
isEmptyRAL (MkR i mixa h)= i == 0

lengthRAL :: RAList a -> Int
--Propósito: devuelve la cantidad de elementos.
--Eficiencia: O(1).
lengthRAL (MkR i mixa h)= if isEmptyRAL then i else i - 1

get :: Int -> RAList a -> a
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
--Eficiencia: O(log N).
get i1 (MkR i mixa h)= case lookupM i1 mixa of
                      Just a  -> a
                      Nothing -> error "El elemento con el indice dado no existe"
{-
    costo: cumple costo O(log N) por lookupM siendo N el indice dado.
-}

minRAL :: Ord a => RAList a -> a
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(1).
minRAL (MkR i mixa h)= findMin h

add :: Ord a => a -> RAList a -> RAList a
--Propósito: agrega un elemento al final de la lista.
--Eficiencia: O(log N).
add x (MkR i mixa h)= let newi= i + 1
                      in  MkR newi (assocM i x) (insertH x)

elems :: Ord a => RAList a -> [a]
--Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
--Eficiencia: O(N log N).
elems (MkR i mixa h)= todosLosElementos h 

todosLosElementos :: Heap a -> [a]
todosLosElementos h = if isEmptyH h then [] else findMin h : todosLosElementos (deleteMin h)

remove :: Ord a => RAList a -> RAList a
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(N log N).
remove (MkR i mixa h) = if isEmptyH
                            then error "La heap se encuentra vacia."
                            else let mina = fromJust (lookupM (i - 1) mixa)
                                in MkR (i -1) (deleteM (i-1) mixa) (sacarMinimo mina h)
                             

sacarMinimo :: a -> Heap a -> Heap a 
sacarMinimo a h = let minH = findMin h 
                    in if a == minH
                        then deleteMin h
                        else insertH minh (sacarMinimo a (deleteMin h))
{-
data RAList a = MkR Int (Map Int a) (Heap a)
-}
set :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
--Eficiencia: O(N log N).
set i1 a (MkR i mixa h)= if isEmptyH
                          then error""
                          else 

addAt :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
--Eficiencia: O(N log N).
--Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
--también como argumento la máxima posición posible.
