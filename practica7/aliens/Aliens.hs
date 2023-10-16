module Aliens

where

data Tree a = NodeT a (Tree a) (Tree a) | EmptyT
        deriving Show


conLaSumaCalculada :: Tree Int-> Tree Int
--Propósito: dado un árbol binario computa un árbol con misma estructura, pero en el que cada nodo posee la suma de los
--elementos de todo su subárbol (incluyendo al elemento en su raíz).
--Precondición: no tiene.
conLaSumaCalculada      EmptyT     = EmptyT
conLaSumaCalculada (NodeT n ti td )= let sumaHijoIzq = conLaSumaCalculada ti
                                         sumaHijoDer = conLaSumaCalculada td
                                         sumaTotal = n + nodoDe sumaHijoDer + nodoDe sumaHijoIzq
                                    in NodeT sumaTotal sumaHijoIzq sumaHijoDer

nodoDe :: Tree Int -> Int
nodoDe EmptyT = 0
nodoDe (NodeT n ti td) = n

caminoQueSumaMas :: Tree Int-> [Int]
--Propósito: dado un árbol devuelve los elementos del camino cuya suma es mayor al resto de los caminos del árbol. Los
--caminos van desde la raíz hacia las hojas.
--Precondición: no tiene
caminoQueSumaMas    EmptyT      = []
caminoQueSumaMas (NodeT n ti td)= let sumaHijoIzq = nodoDe (conLaSumaCalculada ti)
                                      sumaHijoDer = nodoDe (conLaSumaCalculada td)
                                    in if sumaHijoDer > sumaHijoIzq
                                        then n : caminoQueSumaMas td
                                        else n : caminoQueSumaMas ti
sumaCalculada :: Tree Int
sumaCalculada =(NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3(NodeT 4 EmptyT EmptyT)(NodeT 5 EmptyT EmptyT)))

----------------------------------------------------------------------------------------------------------

nuevoAlien :: String-> Alien
-- Propósito: devuelve un alien que posee la raza dada y que tiene cero habilidades.
habilidades :: Alien-> [String]
-- Propósito: devuelve la lista de habilidades que ese alien posee.
cantHabilidades :: Alien-> Int
 --Propósito: devuelve la cantidad de habilidades que ese alien posee.
raza :: String-> String
 --Propósito: devuelve la raza del alien.
aprender :: String-> Alien-> Alien
 --Propósito: agrega una habilidad al alien.

data Galaxia = ConsG (Map String (PriorityQueue Alien)) (Map String [String])
{-
 De esta representación sabemos que
 el primer Map relaciona razas de aliens con aliens ordenados en base a la cantidad de habilidades que poseen;
 el segundo Map relaciona habilidades con las razas que poseen dicha habilidad.
-}
elMasHabilidosoEntre :: [String]-> Galaxia-> [Alien]
--Propósito: dada una lista de razas devuelve al alien más habilidoso entre dichas razas.
--Precondición: existe al menos un alien para cada raza dada.
elMasHabilidosoEntre   []     glx = []
elMasHabilidosoEntre (s:strs) glx = elAlienMasHablidoso s glx : elMasHabilidosoEntre strs glx

{-
    cosoto:
        O(
            r * (siendo r el total de Razas dadas)
            log S (por elAlienMasHablidoso, siendo S el total de Razas en msxa)
        )O(r log S)
-}

elAlienMasHablidoso :: String -> Galaxia -> Alien
--Precondición: existe al menos un alien para cada raza dada.
elAlienMasHablidoso str (ConsG msxa _) = case lookupM str msxa of 
                                         Nothing -> error "La raza no exite en la galaxia"
                                         Just pAlien -> maxPQ pAlien

{-
    costo:
        O(
            log S + (por lookupM, siendo S el total de Razas en msxa)
            1 (por maxPQ)
        )O(log S)
-}

{-data Galaxia = ConsG (Map String (PriorityQueue Alien)) (Map String [String])-}
--aprender :: String-> Alien-> Alien
enseñarARaza :: String-> String-> Galaxia-> Galaxia
--Propósito: dada una habilidad y una raza enseña dicha habilidad a todos los alien de dicha raza.
--Precondición: la raza existe, pero los alien de esa raza aún no tienen dicha habilidad.
--Nota: la habilidad puede o no ya existir.
enseñarARaza habi ra (ConsG msxa msxs) = let newmsxa = enseniarATodos ra habi msxa
                                             newmsxs = darHablidadARaza ra habi msxs
                                        in ConsG newmsxa newmsxs

enseniarATodos :: String -> String -> Map String (PriorityQueue Alien) -> Map String (PriorityQueue Alien)
enseniarATodos ra habi msxa = case lookupM ra msxa of
                                Nothing -> error "La raza dada no existe en la galaxia"
                                Just pAlien -> let pq =  enseniarACada habi pAlien
                                              in assocM ra pq msxa
{-
    costo:
        O( 
            log R +   (por lookupM, siendo R la cantidad total de razas en msxa)
            log R +   (por assocM, siendo R la cantidad total de razas en msxa)
            A log P (por enseniarACada, siendo A y P el total de elementos en la PriorityQueue valor en msxa)
        )=> 2 log R
         => O(log R + A log P)
-}


enseniarACada :: String -> PriorityQueue Alien -> PriorityQueue Alien
enseniarACada  habi pqAlien = if isEmptyPQ 
                               then emptyPQ 
                               else insertPQ (aprender habi (maxPQ pqAlien)) (enseniarACada habi (deleteMaxPQ pqAlien))

{-
    costo :
        O(
            A *     (siendo A el total de elementos en la PriorityQueue valor en msxa)
            log P + (por inserPq, siendo P la cantidad total de elementos en la PriorityQueue)
            log P   (por deleteMaxPq, siendo P la cantidad total de elementos en la PriorityQueue)
        )O(A log P)
-}
