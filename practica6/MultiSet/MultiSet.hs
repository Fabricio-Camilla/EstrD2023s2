module MultiSet
       (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map

data MultiSet a = MS (Map a Int)


emptyMS :: MultiSet a
--Propósito: denota un multiconjunto vacío.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a --(opcional)
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a --(opcional)
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.
multiSetToList :: MultiSet a -> [(a, Int)]
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.


emptyMS = MS emptyM   -- constante
addMS x (MS mp)        = case lookupM  x mp of        --O(lookupM) + O(assocM)
                            Just  n -> MS (assocM x (n+1) mp)
                            Nothing -> MS (assocM x 1 mp )

ocurrencesMS a (MS mp) = case (lookupM a mp) of
                            Just  n -> n 
                            Nothing -> 0



multiSetToList (MS mp) = mapToList mp 