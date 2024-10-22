import Map
ff :: Map Int Int
ff = assocM 2 4 (assocM 1 2 emptyM)

ff' :: Map Int Int
ff' =  assocM 2 8 $ assocM 3 4 (assocM 4 2 emptyM)
valuesM :: Eq k => Map k v -> [Maybe v]   --O(keys)= O(K)
--Propósito: obtiene los valores asociados a cada clave del map.
--costo: O(K log K) por que a cada k le obtengo el valor.
valuesM mp = todosLosValoresDe (keys mp) mp 

todosLosValoresDe :: Eq k =>  [k] -> Map k v -> [Maybe v]  --O(lookupM)= O(log K)  
todosLosValoresDe []     mp = []
todosLosValoresDe (k:ks) mp = (lookupM k mp) : (todosLosValoresDe ks mp)

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
--costo: O(K) porque solamente utilizo la funcion "keys" y la subtarea "todasLasClaves" es de costo lineal
-- porque busca si se encuentra en la lista dada.
todasAsociadas ks mp = todasLasClaves ks (keys mp)

todasLasClaves :: Eq k => [k] -> [k] -> Bool
todasLasClaves   [k]  ks2 = elem k ks2
todasLasClaves (k:ks) ks2 = elem k ks2 && todasLasClaves ks ks2

listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.  --O(assocM) = O(log K)
--costo: O(log K) por que por cada k se le pide el valor, no recorro otra cosa que no sea K
listToMap     []     = emptyM
listToMap ((k,v):xs) = assocM k v (listToMap xs) 

mapToList :: Eq k => Map k v -> [(k, v)]     
--Propósito: convierte un map en una lista de pares clave valor.
--O(keys) = O(K) 
--O(valoresDe)= O(log K)
-- O(K) + O(log K) = O(K log K)por que por a cada k se le pide el valor.
mapToList mp = valoresDe (keys mp) mp 

valoresDe :: Eq k => [k] -> Map k v -> [(k,v)] --O(lookupM K)= O(log K)
--costo: O(log K) por que a cada k le pido el valor y lo agrega constantemente a la lista de tuplas
valoresDe   []   mp = []
valoresDe (k:ks) mp = case lookupM k mp of
                      Just x -> (k,x) : (valoresDe ks mp)
                      Nothing -> valoresDe ks mp

agruparEq :: Eq k => [(k,v)] -> Map k [v]  --O(n) * O(assocM) + O(lookupM)
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.
agruparEq []          = emptyM
agruparEq ((k,v): xs) = case lookupM k (agruparEq xs) of  
                                  Just x -> assocM k  (v:x) (agruparEq xs)
                                  Nothing-> assocM k  [v] (agruparEq xs) -- lo llamo una vez por cada elemento de la lista

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.
incrementar    []  mp = emptyM
incrementar (k:ks) mp = case lookupM k mp of
                               Just x -> assocM k (x+1) (incrementar ks mp) 
                               Nothing -> incrementar ks mp

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero
mergeMaps mp1 mp2 = agregar (keys mp1) mp1 mp2

agregar :: Eq k => [k] -> Map k v -> Map k v-> Map k v
agregar []     mp1  mp2 = emptyM
agregar (k:ks) mp1  mp2 = case lookupM k mp1 of 
                           Just x -> assocM k x  mp2
                           Nothing -> agregar ks mp1 mp2



