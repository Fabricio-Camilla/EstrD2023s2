import Empresa

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
--Propósito: construye una empresa con la información de empleados dada. Los sectores no tienen empleados.
--Costo: calcular.
comenzarCon secs   []  = consEmpresa
comenzarCon secs (c:cs)= asiganarUnEASectores secs c (agregarEmpleado secs c (agregarSectores secs (comenzarCon secs cs)))

recorteDePersonal :: Empresa -> Empresa
--Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
--Costo: calcular.
recorteDePersonal emp = realizarRecorte (todosLosCUIL emp) emp

convertirEnComodin :: CUIL -> Empresa -> Empresa
--Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
--Costo: calcular.
convertirEnComodin c emp = asiganarUnEASectores (todosLosSectores emp) c emp

esComodin :: CUIL -> Empresa -> Bool
--Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
--Costo: calcular
esComodin c emp = elem (buscarPorCUIL c emp) (empleadosDeLosSectores (todosLosSectores emp) emp)


empleadosDeLosSectores :: [SectorId] -> Empresa -> [Empleado]
empleadosDeLosSectores  []        emp = []
empleadosDeLosSectores (sid:sids) emp = (empleadosDelSector sid emp) : empleadosDeLosSectores sids emp 


realizarRecorte :: [CUIL] -> Empresa -> Empresa
realizarRecorte   []   emp = emp
realizarRecorte (c:cs) emp = borrarEmpleado c (realizarRecorte cs emp)

asiganarUnEASectores :: [SectorId] -> CUIL -> Empresa -> Empresa
asiganarUnEASectores    []      c  emp = emp
asiganarUnEASectores (sid:sids) c  emp = agregarASector sid c (asiganarUnEASectores sids c emp)

agregarSectores :: [SectorId] -> Empresa -> Empresa
agregarSectores  []        emp = emp
agregarSectores (sid:sids) emp = agregarSector sid (agregarSectores sids emp)