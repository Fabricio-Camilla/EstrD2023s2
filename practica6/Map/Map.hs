module Map 
    (Map,emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = MP [(k,v)]

emptyM :: Map k v
--Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.

emptyM = MP []
assocM k v (MP xs) = MP (agregar k v xs)
lookupM k (MP xs) = lookUp k xs
deleteM k (MP xs) = MP (eliminarElValor k xs)
keys (MP xs) = claves xs 


agregar :: Eq k => k ->  v -> [(k,v)] -> [(k,v)]
agregar k v     []        = [(k,v)]
agregar k v ((k1, v1):xs) = if k==k1 then (k, v): xs else (k1,v1) : (agregar k v xs)


lookUp :: Eq k => k -> [(k,v)] -> Maybe v
lookUp k [] = Nothing
lookUp k ((k1,v1):xs) = if k==k1 then Just v1 else lookUp k xs

eliminarElValor :: Eq k => k -> [(k,v)] -> [(k,v)]
eliminarElValor k [] = []
eliminarElValor k ((k1,v1):xs) = if k==k1 then xs else (k1,v1) : eliminarElValor k xs 

claves :: [(k,v)] -> [k]
claves [] = []
claves ((k,v):xs) = k : claves xs