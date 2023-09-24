module QueueV2
   (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Queue [a]
   {-
      INV. REP:
         *firstQ, dequeue no pueden tener una lista vacia
   -}

emptyQ :: Queue a
--Crea una cola vacía.
isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.

{-2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
la eficiencia entre ambas implementaciones.-}

emptyQ              = Queue []    --constante
isEmptyQ  (Queue a) = null a      --constante
enqueue x (Queue a) = Queue (x:a) --constante
firstQ    (Queue a) = elPrimeroQ a (length a) --parcial - lineal
dequeue   (Queue a) = (Queue (sacarQ a (length a)))  --parcial - lineal


sacarQ :: [a] -> Int ->[a]
sacarQ [x]    1 = []
sacarQ (x:xs) n = if n > 1 then x : sacarQ xs (n-1) else sacarQ xs (n-1)

elPrimeroQ :: [a] -> Int -> a
elPrimeroQ [x] 1 = x
elPrimeroQ xs  n = elPrimeroQ (tail xs) (n-1)