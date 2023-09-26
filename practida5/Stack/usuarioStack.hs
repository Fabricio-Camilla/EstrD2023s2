import Stack

ff :: Stack Int
ff = push 3(push 2(push 1 emptyS))


apilar :: [a] -> Stack a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar  []    = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar pila = if isEmptyS pila then [] else (top pila) : desapilar (pop pila)

insertarEnPos :: Int -> a -> Stack a -> Stack a
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos 0 x pila = push x pila
insertarEnPos n x pila = push (top pila) (insertarEnPos (n-1) x (pop pila))
