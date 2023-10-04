module PriorityQueue
    (emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a
--Propósito: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool
--Propósito: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
--Propósito: inserta un elemento en la priority queue.
findMinPQ :: Ord a => PriorityQueue a -> a
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Precondición: parcial en caso de priority queue vacía
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).



emptyPQ  = PQ []
isEmptyPQ (PQ ls)  = null ls
insertPQ x (PQ ls) = PQ(x:ls)
findMinPQ  (PQ ls) = minimum ls
deleteMinPQ (PQ ls) = PQ(borrarMinimo ls)

borrarMinimo  ::Ord a => [a] -> [a]
borrarMinimo []   = []
borrarMinimo (x:xs) = borrar (minimum xs) xs

borrar :: Eq a => a -> [a] -> [a]
borrar x []   = []
borrar x (y:ys) = if x==y then  ys else y : borrar x ys 