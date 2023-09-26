module Stack
   (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where 

data Stack a = Stack [a] Int 

    {-
        INV.REP:
            head, tail no pueden tener lista vacia.
            n es la longitud de la lista
    -}


emptyS :: Stack a Int
--Crea una pila vacía.
isEmptyS :: Stack a Int -> Bool
--Dada una pila indica si está vacía.
push :: a -> Stack a Int -> Stack a Int
--Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a Int -> a
--Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a Int -> Stack a Int
--Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a Int -> Int
--Dada la cantidad de elementos en la pila.
--Costo: constante.

--Esto significa que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
--platos

emptyS               = Stack []    --constante 
isEmptyS (Stack a n) = n== 0      --constante
push x   (Stack a n) = Stack (x:a) (n+1)--constante
top      (Stack a _) = head a      --parcial -constante
pop      (Stack a n) = Stack (tail a) (n-1)--parcial -constante
lenS     (Stack a n) = n    --constante
