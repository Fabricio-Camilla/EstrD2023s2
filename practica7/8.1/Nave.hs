module Nave
    (Nave,)

where 

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

data Nave = N (Map SectorId Sector) 
              (Map Nombre Tripulante)
              (MaxHeap Tripulante)

{-
    N Msxs  Mnxp  Hxt
    INV REP:
        * En Msxs para cada clave SectorId sid, su valor Sector s tiene SectorId sid,
            de tal manera que SectorId s es igual a sid.
        * En Mnxp para cada clave Nombre n, su valor Tripulante t tiene como Nombre n,
            de tal manera que Nombre t es igual a n.
        
        * falta invariente de relacion sector tripulante conoce su sectorid

        * Todos los Tripulantes t en Hxt, aparecen como valor en Mnxp.
        * Todos los Tripulantes t valor en Mnxp, estan como elemento en Hxt.
-}



construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S)
construir    []      = N emptyM emptyM emtpyH
construir (sid:sids) = agregarSectorVacio sid (construir sids)
{-
    Costo:
        por cada sid de la lista de SectorId realiza agregar sectores vacio a la nave.
        O(
            sids * (siendo sids la lista de SectorId dado)
            agregarSectorVacio = O(log K) -> siendo K el total de SectorId de la nave
        )
        sids * O(log K) = O(sids log K) 
        => como la lista sids y el total de claves en la nave son iguales, tiene los mismos elementos,
            siendo su costo O(K log K)
        
-}

agregarSectorVacio :: SectorId -> Nave -> Nave
--PRECOND: El SectorId dado no debe estar en la nave.
agregarSectorVacio sid (N mp1 mp2 h) = case lookupM sid mp1 of 
                                     Just _  -> error "El sector con SectorId dado ya pertenece a la nave"
                                     Nothing ->let s= crearS sid 
                                               in N (assocM sid s mp1) mp2 h
{-
    costo: 
        *crearS  = O(1)
        *lookupM = O(log K) -> K el total de las claves de mp1
        *assocM  = O(log K) -> K el total de las claves de mp1
        como solamente asocio un sector con SectorId dado a la nave, su costo seria
        => 2 O(log K)
        =>   O(log K)

-}

ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(log T)
ingresarT n rn (N mp1 mp2 h) = let t = crearT n rn
                               in (N mp1 (assocM n t mp2) (insertH t h))
    
{-
    costo:
        O(
            log N + (siendo N el total de las claves de mp2)
            log T  (sindo T el total de elementos de h)
        )
        O(log N) + O(log T) = O(log N + log T)
-}

sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log M)
sectoresAsignados n (N mp1 mp2 h)= case lookupM n mp2 of
                                 Just t -> sectoresT t
                                 Nothing -> error "El tripulante de nombre dado no existe en la nave"
{-
    costo:
        O(
            log N (siendo N el total de las claves de mp2)
        )
        O(log N)
-}


datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
datosDeSector sid (N mp1 mp2 h) = case lookupM sid mp1 of
                                  Nothing -> error "El sector dado no esta en la nave"
                                  Just s  -> (tripulanteS s, componenteS s)

{-
    costo:
        O(
            log S (siendo S el total de las claves de mp1)
        )
        O(log s)
-}

tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(log T)
tripulantesN (N mp1 mp2 h) = tripulantesDelHeap h 
--Costo : O(M log M) siendo M el total de elemtnos de h

tripulantesDelHeap :: MaxHeap Tripulante -> [Tripulante]
tripulantesDelHeap h = if isEmptyH h 
                       then   []
                       else (maxH h) : tripulantesDelHeap (deleteMaxH h) 
{-
    costo:
        O(
             M *  (por recursion sobre h, siendo M el total de elementos de h)
            log M (por deleteMaxH,siendo M el total de elementos de h)
        )
        O(M log M)
-}

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector comps sid (N mp1 mp2 h) = case lookupM sid mp1 of
                                         Nothing -> N mp1 mp2 h
                                         Just s -> let newmp1 = asoocM sid (agregarComponentes comps s) mp1
                                                   in (N newmp1 mp2 h)
{-
    costo
        O(
            C + (siendo C el total de los componentes de [Componente])
            log S (siendo S el total de claves en mp1)
        )
        O(C + log S)
-}

agregarComponentes :: [Componente] -> Sector ->  Sector
agregarComponentes      []       s = s
agregarComponentes (comp: comps) s = agregarC comp (agregarComponentes comps s)

{-
    costo
        O(
            C (siendo C el total de los componentes de [Componente])
        )
        O(C)
-}

{-data Nave = N (Map SectorId Sector) 
              (Map Nombre Tripulante)
              (MaxHeap Tripulante)-}

asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
asignarASector n sid (N mp1 mp2 h) = case lookupM sid mp1 of
                                     Nothing -> error "El sector dado no esta en la nave"
                                     Just s -> let newmp1 = assocM sid (agregarT n s) mp1 
                                                   newmp2 = tripulanteAsignadoASector n sid mp2
                                               in (N newmp1 newmp2 h)
{-
    costo
        O(
            log S + (por lookupM sobre S, siendo S el total de las claves de mp1) 
            log S + (por assocM sobre S, siendo S el total de las claves de mp1)
            log T + (por agregarT sobre T, siendo T el total de Nombre del Sector)
            (log N + log I) (por tripulanteAsignadoASector sobre N, siendo N el total de las claves de mp2 
                            y I el total de SectorId del Tripulante, de Nombre n en mp2)
        )
        O(log S + log T + log N + log I)
-}


tripulanteAsignadoASector :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante
tripulanteAsignadoASector n sid mp2 = case lookupM n mp2 of
                                      Nothing -> error "El tripulante dado no existe en la nave"
                                      Just t -> assocM n (asignarS sid t) mp2

{-
    costo
        O(
            log N + (siendo N el total de las claves de mp2)
            log N + (siendo N el total de las claves de mp2)
            log S   (siendo S el total de SectorId del Tripulante)
        )
        O(log N + log S)
-}