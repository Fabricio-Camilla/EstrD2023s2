import Nave
sectores :: Nave -> Set SectorId
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores n = let ss = sectoresDe (tripulantesN n) 
              in unificarTodos ss 
-- ls * N log N 
--    + T             
-- O(T * (N log N))

sectoresDe :: [Tripulante] -> [Set SectorId]
sectoresDe  []   = []
sectoresDe (t:ts) = sectoresT t : sectoresDe ts  
--O(T) (siendo T por recorrer cada tripulante de la lista)

unificarTodos :: [Set SectorId] -> Set SectorId
unificarTodos    [] = emptyS
unificarTodos (s:ss) = unionS s (unificarTodos ss)

{-
costo: unoinS sobre recursion (ls * N log N) (siendo ls la lista de Set SectoreId) 
-}


sectores n =  sectoresDe (tripulantesN n) n 
{-
    O(
        M log M +(por tripulantesN, siendo M la cantidad totales de tripulantes de la nave)
    
        T * (log N + S log S') (por sectoresDe)
    )
    => O(M log M (T* (log N + S log S'))  --como M y T hablan de la misma lista vuela.
    => O(M * (log M + log N + S log S'))  --por que el total de la lista M y N son de la misma longitud
    => O(M * (log M + S log S'))
-}
sectoresDe :: [Tripulante] -> Nave -> Set SectorId
sectoresDe   []   n = emptyS
sectoresDe (t:ts) n = unionS (sectoresAsignados (nombre t) n)  (sectoresDe ts n)

{-
    costo
        O(
            T *       (siendo T la lista de Tripulante dada)
            log N +   (por sectoresAsignados, siendo N el total de Nombres de Tripulante en la nave)
            S log S'  (por unionS entre S y S', siendo S el total de elementos en Set SectorId de Tripulante T y,
                             siendo S' el total de elementos en Set SectorId del llamado recursivo)
            
        ) O(T * (log N + S log S'))
-}

sinSectoresAsignados :: Nave -> [Tripulante]
--Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados n =  let ts = tripulantesN n
                           in tripulantesSinSectores ts 

tripulantesSinSectores :: [Tripulante] -> [Tripulante]
tripulantesSinSectores []      = []
tripulantesSinSectores (t:trs) = if sizeS ((sectoresT t) == 0) 
                                 then tripulanteAsignadoASector trs 
                                 else t : tripulanteAsignadoASector trs


barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles n = let ts = setSectoresDeTodos (tripulantesN n)
              in  obtenerBarrillesDe (componentesDeSectores (setToList ts) n) 

obtenerBarrillesDe :: [Componente] -> [Barril]
obtenerBarrillesDe      []      = []
obtenerBarrillesDe (comp:comps) = (obtenerBarril comp) ++ (obtenerBarrillesDe comps)
                                 
obtenerBarril :: Componente -> [Barril]
--El componeten tiene que ser Almacen
obtenerBarril (Almacen bs) = bs
obtenerBarril     _        =[]

componentesDeSectores :: [SectorId] -> Nave -> [Componete]
componentesDeSectores     []     n = []
componentesDeSectores (sid:sids) n = let (_,c) = datosDeSector sid n 
                                     in c ++ (componentesDeSectores sids n)

setSectoresDeTodos :: [Tripulante] -> Set SectorId
setSectoresDeTodos   []   = emptyS
setSectoresDeTodos (t:ts) = unionS (sectoresT t) (setSectoresDeTodos ts)
        
