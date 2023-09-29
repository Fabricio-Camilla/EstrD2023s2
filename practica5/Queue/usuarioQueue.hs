import QueueV2
--import Queue

ff :: Queue Int
ff = enqueue 3(enqueue 2(enqueue 1 emptyQ))

ff' :: Queue Int
ff' = enqueue 4(enqueue 5(enqueue 2 emptyQ))

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ cola  = if isEmptyQ cola then 0 else 1 + lengthQ (dequeue cola)

queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList cola = if isEmptyQ cola then [] else queueToList (dequeue cola) ++ [firstQ cola] 

unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera
unionQ cola1 cola2 = if isEmptyQ cola2 then cola1 else unionQ (enqueue (firstQ cola2) cola1) (dequeue cola2)


