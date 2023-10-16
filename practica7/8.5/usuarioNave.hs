import NaveV2

tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
tripulantes n = let scs = sectores n
                in setDeTripulante scs n

setDeTripulante :: [Sector] -> Nave -> Set Tripulante
setDeTripulante  []
setDeTripulante (s:ss) n = union (tripulantesDe s n) (setDeTripulante ss n)

--bajaDeTripulante :: Tripulante -> Nave -> Nave
--Propósito: Elimina al tripulante de la nave.
--Pista: Considere reconstruir la nave sin ese tripulante.

