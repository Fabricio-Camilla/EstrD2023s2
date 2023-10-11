module Empresa
       (Empresa,consEmpresa,buscarPorCUIL,empleadosDelSector,todosLosCUIL,
       todosLosSectores,agregarSector,agregarEmpleado,agregarASector,borrarEmpleado)
where

import Map
import Set

type SectorId = Int
type CUIL     = Int
data Empresa  = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

{-
    INV.REP.:
        * si  un empleado pertenece a un sector en el primer map 
            dicho empleado tiene que estar asociado a un cuil en el segundo map.
        * un empleado valor del segundo map, pertenece a los conjutnos asociados valor del primer map.
        * para cada cuil c clave en el segundo map, asociado c a un empleado e. "cuil e" es igual a c.
-}



{- consEmpleado :: CUIL -> Empleado
    Propósito: construye un empleado con dicho CUIL.
    Costo: O(1)
cuil :: Empleado -> CUIL
    Propósito: indica el CUIL de un empleado.
    Costo: O(1)
incorporarSector :: SectorId -> Empleado -> Empleado
    Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
    Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
sectores :: Empleado -> [SectorId]
    Propósito: indica los sectores en los que el empleado trabaja.
    Costo: O(S)
-}
consEmpresa :: Empresa
--Propósito: construye una empresa vacía.
--Costo: O(1)
consEmpresa = ConsE  emptyM  emptyS

buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Propósito: devuelve el empleado con dicho CUIL.
--Costo: O(log E)
buscarPorCUIL c (ConsE sec emple) = case lookupM c emple of 
                                    Just e -> e
                                    Nothing -> "Error no existe el empleado con el CUIL dado"

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E)
empleadosDelSector sId (ConsE sec emple) = case lookupM sId sec of
                                           Just emps -> emps
                                           Nothing   -> "Error no hay empleados asignados ese sector"

todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E)
todosLosCUIL (ConsE sec emple) = keys emple

todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S)
todosLosCUIL (ConsE sec emple) = keys sec

agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS)
agregarSector sId (ConsE secs emple) = case lookupM sId secs of 
                                     Nothing  -> ConsE secs emple
                                     Just sec -> ConsE (assocM sId emptyS sec) emple

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
--Costo: calcular.
agregarEmpleado sIds c (ConsE sec emples) = let e = incorporarSectores sIds (consEmpleado c)
                                            in ConsE (asignarEmpleadoASectores sIds e sec) (assocM c e emples)


incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores     []     e = e
incorporarSectores (sId:sIds) e = incorporarSector sId (incorporarSectores sIds e) 

asignarEmpleadoASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
asignarEmpleadoASectores     []     e mSec = mSec
asignarEmpleadoASectores (sid:sids) e mSec =  case (lookupM sid mSec) of 
                                              Just emples -> assocM sid (addS e emples) (asignarEmpleadoASectores sids e mSec )
                                              Nothing -> asignarEmpleadoASectores sids e mSec
            
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular.
agregarASector sId c (ConsE sec emples) = case (lookupM c emples) of
                                          Nothing -> (ConsE sec emples)
                                          Just e  -> (ConsE (asignarASector e sId sec) emples)

asignarASector :: Empleado -> SectorId -> Map SectorId(Set Empleado) -> Map SectorId(Set Empleado)
asignarASector e sid mSec = case (lookUp sid mSec) of
                            Nothing -> mSec
                            Just emples -> assocM sid (agregarSiNoPertenece e emples)

agregarSiNoPertenece :: Empleado -> Set Empleado -> Set Empleado
agregarSiNoPertenece e emples = if belongs e emples then addS e emples else emples

borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: calcular.
borrarEmpleado c (ConsE sec emples) = ConsE (deleteM c sec) (deleteM c emples)

data Empresa  = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)