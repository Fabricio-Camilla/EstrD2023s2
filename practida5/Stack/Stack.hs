module Stack
   (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where 

data Stack a = Stack [a]

    {-
        INV.REP:
            head, tail no pueden tener lista vacia.
    -}


emptyS :: Stack a
--Crea una pila vacía.
isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
--Dada la cantidad de elementos en la pila.
--Costo: constante.

--Esto significa que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
--platos

emptyS             = Stack []    --constante 
isEmptyS (Stack a) = null a      --constante
push x   (Stack a) = Stack (x:a) --constante
top      (Stack a) = head a      --parcial -constante
pop      (Stack a) = Stack (tail a) --parcial -constante
lenS     (Stack a) = length a    --parcial - lineal
