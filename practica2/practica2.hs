--ej1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns
--ej2
longitud :: [a] -> Int
longitud []     = 0
longitud (a:as) = 1 + longitud as

--ej3
sucesor :: [Int] -> [Int]
sucesor []     = []
sucesor (n:ns) = n + 1 : sucesor ns

--ej4
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (x:xs) = x  && conjuncion xs

--ej5
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (x:xs) = x  || disyuncion xs

--ej6
aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (a:as) = a ++ aplanar as

--ej7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False 
pertenece a (x:xs) = x == a || pertenece a xs

--ej8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones a (x:xs) = unoSiCeroSiNo (a == x) + apariciones a xs


--ej9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA  _ []     = []
losMenoresA  n (x:xs) = if n>x 
                        then x : losMenoresA n xs
                        else losMenoresA n xs
--ej10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []   
lasDeLongitudMayorA n (a:as) =  if longitud a > n
                                then a : lasDeLongitudMayorA n as 
                                else lasDeLongitudMayorA n as
    
--ej11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal  []    x = [x]
agregarAlFinal (a:as) x = a : agregarAlFinal as x 

--ej12
agregar :: [a] -> [a] -> [a]
agregar   []   bs = bs
agregar (a:as) bs = a  : agregar as bs

--ej13
reversa :: [a] -> [a]
reversa []     = []
reversa (a:as) = agregar(reversa as)  [a]

--ej14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos  ns       []    = ns
zipMaximos  []       ms    = ms
zipMaximos (n:ns) (m:ms)   = if n>m 
                           then n : zipMaximos ns ms
                           else m : zipMaximos ns ms


--zipMaximos [1,2,3,4,5] [2,3,5]
--2:(3: zipMaximos [4,5] [3,5])
--2:3:5:[4,5]

--ej15
elMinimo :: Ord a => [a] -> a
--PRECOND: No puede ser una lista vacia
elMinimo   []   = error "No hay elementos"
elMinimo (a:[]) = a
elMinimo (a:as) = min a (elMinimo as)

--recursiion sobre numeros 
--ej1
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) 

--ej2 
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = if n > 0 then n : cuentaRegresiva (n-1) else []

--ej3
repetir :: Int -> a -> [a]
repetir 0 a = []
repetir n a = a : repetir (n-1) a 

--ej4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0   _    = []
losPrimeros _   []   = []
losPrimeros n (a:as) = a : losPrimeros (n-1) as

--ej5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0    as   = as
sinLosPrimeros _    []   = []
sinLosPrimeros n  (a:as) =  sinLosPrimeros (n-1) as 

--registros


data Persona = P String  Int
            --  nombre  edad
  deriving Show

sho= (P "Sho" 12)
sho2= (P "Sho" 24)
--ej1
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _   []   = []
mayoresA e (x:xs) = if esMayor x e
                    then x : mayoresA e xs
                    else mayoresA e xs

esMayor :: Persona -> Int -> Bool
esMayor (P _ e1) e2 = e1 > e2

--ej2
promedioEdad :: [Persona] -> Int
        --PRECOND: La lista debe tener al menos una persona
promedioEdad [] = error "La lista no debe estar vacia"  
promedioEdad p  = div (sumatoriaEdad p) (length p) 

sumatoriaEdad :: [Persona] -> Int
sumatoriaEdad    []  =  0
sumatoriaEdad (x:xs) = edad x + sumatoriaEdad xs

edad :: Persona -> Int
edad (P _ e) = e

--ej3
elMasViejo :: [Persona] -> Persona
          --PRECOND: La lista debe tener al menos una persona
elMasViejo  []    = error "La lista no debe estar vacia"
elMasViejo  [p]   = p
elMasViejo (x:xs) = if edad x > edad (elMasViejo xs)
                    then x
                    else elMasViejo xs 

-- pokemon

data TipoDePokemon = Agua | Fuego | Planta
   deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]


--ej1
entrenador1= (ConsEntrenador "el" [poke1, poke2, poke3])
entrenador2= (ConsEntrenador "2el" [poke2])
poke1= (ConsPokemon Agua 10)
poke2= (ConsPokemon Fuego 30)
poke3= (ConsPokemon Planta 30)

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = length ps
--ej2
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = sumarSiEsDelMismoTipo t ps

sumarSiEsDelMismoTipo :: TipoDePokemon -> [Pokemon] -> Int
sumarSiEsDelMismoTipo _   []   = 0
sumarSiEsDelMismoTipo t (p:ps) = unoSiCeroSiNo(sonDelMismoTipo t (tipoDe p)) + sumarSiEsDelMismoTipo t ps

-- sumarSiEsDelMismoTipo :: TipoDePokemon -> [Pokemon] -> Int
-- sumarSiEsDelMismoTipo t   []   = 0
-- sumarSiEsDelMismoTipo t (p:ps) = if (sonDelMismoTipo t (tipoDe p))
                                --    then 1 + sumarSiEsDelMismoTipo t ps
                                --    else  sumarSiEsDelMismoTipo t ps 

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _ ) = t

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo Agua   Agua    = True
sonDelMismoTipo Fuego  Fuego   = True
sonDelMismoTipo Planta  Planta = True
sonDelMismoTipo   _         _  = False
  
unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo True = 1
unoSiCeroSiNo   _  = 0

--ej3
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_  t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) = losPokemonDeTipoQueLeGana t ps1 ps2


losPokemonDeTipoQueLeGana :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
losPokemonDeTipoQueLeGana t    []     _   =  0
losPokemonDeTipoQueLeGana t (p1:ps1)  ps2  = if sonDelMismoTipo t (tipoDe p1) 
                                             then unoSiCeroSiNo(leGanaATodos p1 ps2) + losPokemonDeTipoQueLeGana t ps1 ps2
                                             else losPokemonDeTipoQueLeGana t ps1 ps2

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos  p   []     =  True
leGanaATodos  p (p1:ps1) =  leGanaPorTipo (tipoDe p) (tipoDe p1) && leGanaATodos p ps1

tieneMasEnergia :: Pokemon -> Pokemon -> Bool
tieneMasEnergia p1 p2 = energia p1 > energia p2

energia :: Pokemon -> Int
energia (ConsPokemon _ e) = e


leGanaPorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
leGanaPorTipo Agua   Fuego  = True
leGanaPorTipo Fuego  Planta = True
leGanaPorTipo Planta Agua   = True
leGanaPorTipo   _      _    = False

--ej4
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = (tieneUnoDelTipo Planta ps) && (tieneUnoDelTipo Agua ps) && (tieneUnoDelTipo Fuego ps)

tieneUnoDelTipo :: TipoDePokemon -> [Pokemon] -> Bool
tieneUnoDelTipo _   []   = False
tieneUnoDelTipo t (p:ps) = sonDelMismoTipo t (tipoDe p) || tieneUnoDelTipo t ps



--empresa

data Seniority = Junior | SemiSenior | Senior
      deriving Show
data Proyecto  = ConsProyecto String
    deriving Show
data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto
   deriving Show 
data Empresa   = ConsEmpresa [Rol]
  deriving Show
empresa1= ConsEmpresa [Developer Senior proyecto1, Developer Junior proyecto1]
proyecto1= ConsProyecto "miProy"
proyecto2= ConsProyecto "2proy"

--ej1
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosDeRolesSinRepetidos rs

proyectosDeRolesSinRepetidos :: [Rol] -> [Proyecto]
proyectosDeRolesSinRepetidos   []   = []
proyectosDeRolesSinRepetidos (r:rs) = agregarSinRepetir (proyectoDe r) (proyectosDeRolesSinRepetidos rs)

agregarSinRepetir :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSinRepetir p  []    = [p]
agregarSinRepetir p (x:xs) = if sonPoryectosIguales x p  
                            then agregarSinRepetir p xs
                            else x : agregarSinRepetir p xs

sonPoryectosIguales :: Proyecto -> Proyecto -> Bool
sonPoryectosIguales (ConsProyecto n1) (ConsProyecto n2) = n1 == n2

--ej2
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = length (trabajaEn(losSenior(losDevs rs)) ps)

losDevs :: [Rol] -> [Rol]
losDevs  [] = []
losDevs (r:rs) = if esDev r
                 then r : losDevs rs
                 else losDevs rs

losSenior :: [Rol] -> [Rol]
losSenior   []   = []
losSenior (r:rs) = if esSenior r
                  then r : losSenior rs
                  else losSenior rs

esSenior :: Rol -> Bool
esSenior (Developer Senior _)  = True
esSenior (Management Senior _) = True
esSenior       _               = False

esDev :: Rol -> Bool
esDev (Developer _ _)  = True
esDev (Management _ _) = False
esDev  _               = False

--ej3
trabajaEn :: [Rol] -> [Proyecto] -> [Rol]
trabajaEn  []    ps = []
trabajaEn (r:rs) ps = if rolTrabajaEn r ps
                      then r : (trabajaEn rs ps)
                      else trabajaEn rs ps

rolTrabajaEn :: Rol -> [Proyecto] -> Bool
rolTrabajaEn r   []   = False
rolTrabajaEn r (p:ps) = sonPoryectosIguales (proyectoDe r) p  || rolTrabajaEn r ps

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p)  = p
proyectoDe (Management _ p) = p

--ej 3
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn  []            _          = 0
cantQueTrabajanEn proyectos(ConsEmpresa rs) = length (trabajaEn rs proyectos)


--ej 4
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) =  contarPersonaDeCadaProyecto (proyectosDeRolesSinRepetidos rs) rs

contarPersonaDeCadaProyecto :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
contarPersonaDeCadaProyecto []     _  = []
contarPersonaDeCadaProyecto (p:ps) rs = (proyectoDeYCantidadDePersonas p rs) : (contarPersonaDeCadaProyecto ps rs)

proyectoDeYCantidadDePersonas :: Proyecto -> [Rol] -> (Proyecto, Int)
proyectoDeYCantidadDePersonas  p   []   = (p,0)
proyectoDeYCantidadDePersonas  p (r:rs) = if (sonPoryectosIguales p (proyectoDe r))
                                        then (p, snd (proyectoDeYCantidadDePersonas p rs) + 1) 
                                        else  (proyectoDeYCantidadDePersonas p rs)


