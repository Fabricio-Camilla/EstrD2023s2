programasEnComun :: Persona-> Persona-> Organizador-> Set Checksum
--Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
--programaron juntas.
programasEnComun  p1 p2 org = intersection (programasDe org p1) (programasDe p2 org)

esUnGranHacker :: Organizador-> Persona-> Bool
--Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador
esUnGranHacker org p1 = esAutorDeTodos (todosLosProgramas org) p1 org 

esAutorDeTodos :: [Checksum] -> Persona -> Organizador -> Bool
esAutorDeTodos    []   p1 org = True
esAutorDeTodos (c:chs) p1 org = belongs p1 (autoresDe org c) && esAutorDeTodos chs p1 org
{-
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
-}


