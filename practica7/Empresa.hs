module Empresa
       (Empresa,)
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
buscarPorCUIL c (ConsE sec emple) = empleadoDeCUIL c emple

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E)
todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E)
todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S)
agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS)
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
--Costo: calcular.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular.
borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: calcular.


empleadoDeCUIL :: CUIL -> [Empleado] -> Empleado
empleadoDeCUIL n    []    = 
empleadoDeCUIL n (e:emps) = if n == (cuil e) then e else empleadoDeCUIL n emps