module Queue
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

{-1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
final de la lista y desencolarse por delante-}

emptyQ              = Queue []         --constante
isEmptyQ  (Queue a) = null a           --constante
enqueue x (Queue a) = Queue (a ++ [x]) --lineal 
firstQ    (Queue a) = head a           --parcial - constante
dequeue   (Queue a) = (Queue (tail a)) --parcial - constante




---------------------------
--queue con dos listas
emptyQ :: Queue a
emptyQ = QQ [] []
-- si fs esta vacia bs tambien esta vacia
-- fs primero lista, bs segunda lista


isEmptyQ  :: Queue a -> Bool
isEmptyQ (QQ fs _) = null fs

firstQ :: Queue a -> a
firstQ (QQ fs _)= head fs

enqueue :: a -> Queue a -> Queue a
enqueue x (QQ fs bs) = if null fs then QQ (x:fs) bs else  QQ fs (x:bs)

dequeue :: Queue a -> Queue a
dequeue (QQ fs bs) = if (null (tail fs)) then QQ(reverse bs) [] else QQ(tail fs) bs 








