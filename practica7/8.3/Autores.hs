module Autores
where

data Organizador = MkO (Map Checksum (Set Persona)) 
                        (Map Persona (Set Checksum))

{-
    INV REP. MkO Mcxp Mpxc
        * Toda Persona P valor en Mcxp, tiene asociado un Checksum C
        * Todo Checksum C valor en Mpxc, tiene asociado una Persona P
        * Todo elemento Persona P valor en Mcxp, se encuentra asociado a un Checksum C,
            que por lo tanto se encuentra como valor en Mpxc que es igual a C.
        * Todo elemento Checksum C valor en Mpxc, se encuentra asociado una Persona P,
            que por lo tanto se encuentra como valor en Mcxp que es igual a P.
-}

nuevo :: Organizador
--Propósito: Un organizador vacío.
--Eficiencia: O(1)
nuevo = emptyM emptyM
agregarPrograma :: Organizador-> Checksum-> Set Persona-> Organizador
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (MkO mcxp mpxc) chk pers = case lookupM chk mcxp 
                                            Just _  -> error "El programa dado ya se encuentra en la organizacion"
                                            Nothing -> let newmcxp = assocM chk pers mcxp
                                                        in MkO newmcxp (asignarCheckAPersonas (setToList pers) chk mpxc)

asignarCheckAPersonas :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
asignarCheckAPersonas  []     chk mpxc = mpxc
asignarCheckAPersonas (p:ps)  chk mpxc = let schk = fromJust(lookupM p mpxc) 
                                            newSPer = (addS chk schk)
                                         in assocM p newSPer (asignarCheckAPersonas ps chk mpxc)
                                       

todosLosProgramas :: Organizador-> [Checksum]
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MkO mcxp _) = domM mcxp

autoresDe :: Organizador-> Checksum-> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(logC) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe 

{-data Organizador = MkO (Map Checksum (Set Persona)) 
                        (Map Persona (Set Checksum))-}
programasDe :: Organizador-> Persona-> Set Checksum
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(logP) en peor caso, donde P es la cantidad total de personas del organizador.
programaronJuntas :: Organizador-> Persona-> Persona-> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas.
--Eficiencia: O(logP + ClogC) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
nroProgramasDePersona :: Organizador-> Persona-> Int
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--Eficiencia: O(logP) en peor caso, donde P es la cantidad de personas del organizador.