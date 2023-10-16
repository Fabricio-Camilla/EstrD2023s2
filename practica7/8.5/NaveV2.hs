module NaveV2
    (NaveV2,)
where

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

{-  MKN Msxt Ht Tsi
    INV REP.:
        * Toda clave Sector s en Msxt, esta asociado a un conjunto de Tripulante T,
            por lo tanto el Sector S de Tripulante t es igual a s.
        * Todos los Tripulantes T en Ht, aparecen como valor en Msxt.
        * Todos los Tripulantes T que pertenezcan al conjunto de valor en Msxt, estan como elemento en Ht.
-}


naveVacia :: [Sector] -> Nave
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia  secs = let sectoresNuevos= todosLosSectores secs
                  in MkN sectoresNuevos emtpyH (head secs, 0)

todosLosSectores :: [Sector] -> Map Sector (Set Tripulante)
todosLosSectores   []   = emptyM
todosLosSectores (s:ss) = assocM s emptyS (todosLosSectores ss)
{-
    costo:
        O(
            S*    (la lista de sectores dada)
            log S (por assocM, siendo S el total de sectores de la lista)
        )O(S log S)
-}

tripulantesDe :: Sector -> Nave -> Set Tripulante
--Propósito: Obtiene los tripulantes de un sector.
--Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe s (MkN msxt h tsi)= case lookupM s msxt of 
                                 Just trip -> trip
                                 Nothing -> error "El sector no se encuentra en la nave"
--costo: por lookupM de costo O(log s) siendo s el sector a saber los tripulantes.

sectores :: Nave -> [Sector]
--Propósito: Denota los sectores de la nave
--Costo: O(S) siendo S la cantidad de sectores.
sectores (MkN msxt h tsi) = domM msxt
--costo: por domM de costo O(S) siendo S la cantidad de sectores

conMayorRango :: Nave -> Tripulante
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
--Costo: O(1) por findMin. 
conMayorRango (MkN msxt h tsi)= if isEmptyH then error "La nave no tiene tripulantes" else findMin h

conMasTripulantes :: Nave -> Sector
--Propósito: Denota el sector de la nave con más tripulantes.
--Costo: O(1).
conMasTripulantes (MkN msxt h tsi)= fst tsi

conRango :: Rango -> Nave -> Set Tripulante
--Propósito: Denota el conjunto de tripulantes con dicho rango.
--Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango r (MkN msxt h tsi)= tripulantesDeRango r h

tripulantesDeRango :: Rango -> Heap Tripulante -> Set Tripulante
tripulantesDeRango r   hT   = if isEmptyH h 
                            then emptyS
                            else let    minH    = findMin h
                                    tripDeRango = tripulantesDeRango r (deleteMinH h)  
                            in if r == rango minH  
                                  then addS minH tripDeRango
                                  else tripDeRango
{-
    costo:
        O(
            T *   (siendo T la cantidad de Tripulantes en hT)
            log T (por addS, siendo T la totalidad de los tripulantes de hT)
            log N (por deleteMinH, siendo T cantidad de tripulantes en hT)
        )=> O(T log T)
-}

{-data Nave = MkN (Map Sector (Set Tripulante)) 
                    (Heap Tripulante)
                    (Sector, Int)-}
sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe trip n = sectorDelTripEn n trip (domM n)

sectorDelTripEn :: Nave -> Tripulante -> [Sector] -> Sector
sectorDelTripEn (MkN msxt h tsi) trip scs = tripulanteEnSector msxt trip scs

tripulanteEnSector :: Map Sector(Set Tripulante) -> Tripulante -> [Sector] -> Sector
tripulanteEnSector msxt trip []      = error "El tripulante dado no existe en la nave"
tripulanteEnSector msxt trip (s:scs) = case lookupM s msxt
                                   Just strip -> if belongs trip strip then s else tripulanteEnSector msxt trip scs
                                    Nothing -> error " El tripulante dado no existe en la nave"

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
--Costo: No hay datos (justifique su elección).
--Los tipos T ripulante, Rango y Sector son abstractos. Pero sabemos que son comparables (Ord, Eq) y que el tipo T ripulante
--posee esta función en su interfaz (la única que nos interesa):
