estaVacia :: [a] -> Bool
estaVacia []  = True
estaVacia [x] = False

elPrimero :: [a] -> a
elPrimero (x: _ ) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : x) = x

splitHead :: [a] -> (a,[a])
splitHead x = ((elPrimero x),( sinElPrimero x))