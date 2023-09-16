data Pizza = Prepizza| Capa Ingrediente Pizza
   deriving Show

data Ingrediente = Salsa| Queso| Jamon| Aceitunas Int
     deriving Show


pizza1= Capa Salsa (Capa Queso Prepizza)
pizza2 = (Capa (Aceitunas 2) Prepizza)

--ej1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza      = 0
cantidadDeCapas (Capa ing pi) = 1 + cantidadDeCapas pi

--ej2
armarPizza :: [Ingrediente] -> Pizza
armarPizza    []       = Prepizza
armarPizza (ing: ings) = Capa ing (armarPizza ings)

--ej3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza      = Prepizza
sacarJamon (Capa ing pi) = if (esJamon ing) then sacarJamon pi else (Capa ing (sacarJamon pi))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon    _  = False

--ej4
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza  = True
tieneSoloSalsaYQueso pi        = tieneSalsa pi && tieneQueso pi

tieneSalsa :: Pizza -> Bool
tieneSalsa Prepizza      = True
tieneSalsa (Capa ing pi) = esSalsa ing || tieneSalsa pi 

tieneQueso :: Pizza -> Bool
tieneQueso Prepizza      = True
tieneQueso (Capa ing pi) = esQueso ing || tieneQueso pi 


esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso    _  = False


esSalsa :: Ingrediente -> Bool
esSalsa Queso = True
esSalsa    _  = False


--ej5
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza      = Prepizza
duplicarAceitunas (Capa ing pi) = if (esAceituna ing) then Capa (duplicarIngredientes ing) (duplicarAceitunas pi)  else duplicarAceitunas pi

duplicarIngredientes :: Ingrediente -> Ingrediente
duplicarIngredientes  (Aceitunas n) = Aceitunas (n*2)


esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas n ) = True
esAceituna    _  = False


--ej6
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza    []   = []
cantCapasPorPizza  (p:ps) = (cantidadDeCapas p , p) :  cantCapasPorPizza ps 


data Dir = Izq | Der
   deriving Show
data Objeto = Tesoro | Chatarra
   deriving Show
data Cofre = Cofre [Objeto]
     deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

camino =  (Bifurcacion (Cofre []) (Fin (Cofre[])) (Fin (Cofre[Tesoro])))

--ej1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin cof)              = hayTerosoroAhora cof
hayTesoro (Bifurcacion co m1 m2 )= hayTerosoroAhora co || hayTesoro m1 || hayTesoro m2 

hayTerosoroAhora :: Cofre -> Bool
hayTerosoroAhora (Cofre obs) = tieneTesoro obs
hayTerosoroAhora         _   = False

tieneTesoro :: [Objeto] -> Bool
tieneTesoro []     = False
tieneTesoro (x:xs) =  esTesoro x ||  tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro    _   = False

--ej2

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn    []               a              = hayTerosoroAhoraEnMapa a 
hayTesoroEn    _              (Fin _)          = False
hayTesoroEn (dir:dirs) (Bifurcacion cof m1 m2) = if esIzquierda dir 
                                                then hayTesoroEn dirs m1 
                                                else hayTesoroEn dirs m2 

esIzquierda :: Dir -> Bool
esIzquierda  Izq = True
esIzquierda   _  = False

hayTerosoroAhoraEnMapa :: Mapa -> Bool
hayTerosoroAhoraEnMapa (Fin cof)             = hayTerosoroAhora cof
hayTerosoroAhoraEnMapa (Bifurcacion cof _ _) = hayTerosoroAhora cof


--ej3
caminoAlTesoro :: Mapa -> [Dir]
--Precond: existe un tesoro y es unico
caminoAlTesoro (Fin cof)               = hayTesoroOError cof 
caminoAlTesoro (Bifurcacion cof m1 m2) =  if hayTerosoroAhora cof
                                          then []
                                          else if hayTesoro m1 
                                                then Izq : (caminoAlTesoro m1) 
                                                else Der : (caminoAlTesoro m2)

hayTesoroOError:: Cofre -> [a]
hayTesoroOError (Cofre obs) = if tieneTesoro obs
                              then []
                              else error "Debe haber al menos un tesoro" 

--ej4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]  
caminoDeLaRamaMasLarga (Fin cof)               =  []
caminoDeLaRamaMasLarga (Bifurcacion cof m1 m2) = if length (caminoDeLaRamaMasLarga m1) > length (caminoDeLaRamaMasLarga m2)
                                                 then Izq : caminoDeLaRamaMasLarga m1
                                                 else Der : caminoDeLaRamaMasLarga m2



--ej5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cof)              = [tesorosDe cof]
tesorosPorNivel (Bifurcacion cf m1 m2) = tesorosDe cf : (unificar (tesorosPorNivel m1) (tesorosPorNivel m2))
   
unificar :: [[Objeto]] -> [[Objeto]] ->[[Objeto]]   
unificar    []           objs      = objs
unificar    objs          []       = objs 
unificar (obs:obss) (objs: objss) = (obs ++ objs) : unificar obss objss 


tesorosDe :: Cofre -> [Objeto]
tesorosDe (Cofre obs) = agregarTesoro obs 


agregarTesoro :: [Objeto] -> [Objeto]
agregarTesoro   []   = []
agregarTesoro  (ob: obs)  = if esTesoro ob then ob : agregarTesoro obs else  agregarTesoro obs


--ej6 
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin cof)               = []
todosLosCaminos (Bifurcacion cof m1 m2) = agregarATodos Izq  (todosLosCaminos m1) ++ agregarATodos Der (todosLosCaminos m2)

agregarATodos :: Dir -> [[Dir]] -> [[Dir]]
agregarATodos dir    []    = [dir] : []
agregarATodos dir (dr:dss) = (dir : dr ) : agregarATodos dir dss

cofre1 = (Cofre [Chatarra,Tesoro])
cofre2 = (Cofre [Chatarra, Chatarra ])
cofre3 = (Cofre [Chatarra, Chatarra, Chatarra,Tesoro])
cofre4 = (Cofre [Tesoro, Tesoro])
cofre5 = (Cofre [Tesoro, Chatarra, Tesoro])


mapa1 = (Bifurcacion cofre2 (Fin cofre1) (Fin cofre3))
mapa3 = (Bifurcacion cofre2 (Bifurcacion cofre2 mapa1 (Bifurcacion cofre3 mapa1 (Fin cofre4))) (Fin cofre4))


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
     deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show
--nave=N(NodeT S("sector1",[LanzaTorpedos], ["sho"]) S("sector2", [LanzaTorpedos], ["sho2"]) )

--ej1
sectores :: Nave -> [SectorId]
   --Propósito: Devuelve todos los sectores de la nave.
sectores (N tr) = idDeLosSectores tr 

idDeLosSectores :: Tree Sector -> [SectorId]
idDeLosSectores EmptyT              =  []
idDeLosSectores (NodeT s sizq sder) =  idDelSector s : idDeLosSectores sizq ++ idDeLosSectores sder

idDelSector :: Sector -> SectorId
idDelSector (S sid comps trip) = sid

--ej2
poderDePropulsion :: Nave -> Int
   --Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave
poderDePropulsion (N tr) = cantidadDePropulsion tr

cantidadDePropulsion :: Tree Sector -> Int
cantidadDePropulsion EmptyT          = 0
cantidadDePropulsion (NodeT s t1 t2) = motorDelSector s + cantidadDePropulsion t1 + cantidadDePropulsion t2

motorDelSector :: Sector -> Int
motorDelSector (S sid comps trip) = motoresDeLosComponentes comps

motoresDeLosComponentes :: [Componente] -> Int
motoresDeLosComponentes      []       = 0
motoresDeLosComponentes  (comp:comps) = if esMotor comp
                                        then poderDelMotor comp + motoresDeLosComponentes comps
                                        else motoresDeLosComponentes comps

esMotor :: Componente -> Bool
esMotor (Motor x) = True
esMotor   _   = False

poderDelMotor :: Componente -> Int
poderDelMotor (Motor hp) = hp

barriles :: Nave -> [Barril]
   --Propósito: Devuelve todos los barriles de la nave
barriles (N tr) = barrilesDeLosSectores tr

barrilesDeLosSectores :: Tree Sector -> [Barril]
barrilesDeLosSectores EmptyT          = []
barrilesDeLosSectores (NodeT s t1 t2) = componentesBarrilDelSector s ++ barrilesDeLosSectores t1 ++ barrilesDeLosSectores t2

componentesBarrilDelSector :: Sector -> [Barril]
componentesBarrilDelSector (S sid comps trip) = componentesBarriles comps

componentesBarriles :: [Componente] -> [Barril]
componentesBarriles   []          = []
componentesBarriles  (comp:comps) = if esAlmacen comp
                                    then (barrilesDe comp) ++ componentesBarriles comps
                                    else componentesBarriles comps

esAlmacen :: Componente -> Bool
esAlmacen (Almacen bars)  = True
esAlmacen       _         = False

barrilesDe :: Componente -> [Barril]
barrilesDe  (Almacen bars) = bars


--ej4
   --Propósito: Añade una lista de componentes a un sector de la nave
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector comps sId (N tr) = (N (extenderSector comps sId tr))


extenderSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
extenderSector comps sId EmptyT           = EmptyT
extenderSector comps sId (NodeT s t1 t2)  =  if sId == idDelSector s 
                                             then (NodeT (agregarLe comps s) t1 t2 )
                                             else (NodeT s (extenderSector comps sId t1) (extenderSector comps sId t2 ))

agregarLe :: [Componente] -> Sector -> Sector
agregarLe  []          s            =  s
agregarLe  comps  (S id comPs trip) =  (S id (comps ++ comPs) trip)


--ej5
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA trip sIds (N tr) =  (N (ingresarTripALaNave trip sIds tr))

ingresarTripALaNave :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
ingresarTripALaNave trip []         tr = tr
ingresarTripALaNave trip (sid:sids) tr = agregarTripAlSectorDeId trip sid (ingresarTripALaNave trip sids tr)

agregarTripAlSectorDeId :: Tripulante -> SectorId -> Tree Sector -> Tree Sector
agregarTripAlSectorDeId trip sid EmptyT          = EmptyT
agregarTripAlSectorDeId trip sid (NodeT s t1 t2) = (NodeT (agregaTripASector trip sid s) (agregarTripAlSectorDeId trip sid t1) (agregarTripAlSectorDeId trip sid t2))

agregaTripASector :: Tripulante -> SectorId -> Sector -> Sector
agregaTripASector trip sid (S id comps trips) = if sid == id
                                                then (S id comps (trip : trips)) 
                                                else (S id comps trips)


--ej6
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados trip (N tr) = tripulantesEn tr trip

tripulantesEn :: Tree Sector -> Tripulante -> [SectorId]
tripulantesEn EmptyT          trip = []
tripulantesEn (NodeT s t1 t2) trip = (idDelSectorQueEsta s trip) ++ (tripulantesEn t1 trip) ++ (tripulantesEn t2 trip)

idDelSectorQueEsta :: Sector -> Tripulante -> [SectorId]
idDelSectorQueEsta (S id comps trips) trip = if elem trip trips 
                                             then [id]
                                             else []

--ej7
tripulantes :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N tr) = sinRepetidos(tripulanteEn tr)

sinRepetidos :: [Tripulante] -> [Tripulante]
sinRepetidos  []           = []
sinRepetidos (trip: trips) = if elem trip trips
                              then sinRepetidos trips
                              else trip : sinRepetidos trips

tripulanteEn :: Tree Sector -> [Tripulante]
tripulanteEn     EmptyT      =  []
tripulanteEn (NodeT s t1 t2) = tripulantesDelSector s ++ tripulanteEn t1 ++ tripulanteEn t2

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S id comps trips) = trips



treeA_2 = N(NodeT sector_5 (NodeT sector_7 (NodeT sector_3 (EmptyT) (EmptyT)) 
                                          (NodeT sector_4 (EmptyT) (EmptyT))) 
                          (NodeT sector_2 (NodeT sector_8 (EmptyT) (EmptyT)) 
                                          (NodeT sector_1 (EmptyT) (EmptyT))))


sector_1 = (S sectorId_1 [componente_1, componente_4] [tripulante_7, tripulante_1])
sector_2 = (S sectorId_2 [componente_2, componente_3] [tripulante_4, tripulante_5])
sector_3 = (S sectorId_3 [componente_1, componente_2] [tripulante_3, tripulante_7])
sector_4 = (S sectorId_4 [componente_2, componente_4] [tripulante_2, tripulante_1])
sector_5 = (S sectorId_5 [componente_6, componente_4] [tripulante_6, tripulante_5])
sector_6 = (S sectorId_5 [componente_6, componente_4] [tripulante_2, tripulante_1])
sector_7 = (S sectorId_5 [componente_6, componente_4] [tripulante_3, tripulante_6])
sector_8 = (S sectorId_5 [componente_6, componente_4] [tripulante_6, tripulante_1])

tripulante_1 = "Miguel miguelo"
tripulante_2 = "Otro miguel"
tripulante_3 = "Tercer tripulante generico"
tripulante_4 = "Pedro Pascal"
tripulante_5 = "Papa noel"
tripulante_6 = "El conejo de pascua"
tripulante_7 = "otro tripulante generico"

sectorId_1 = "Este es el primer sector"
sectorId_2 = "Este es el segundo sector"
sectorId_3 = "Este es el tercer sector"
sectorId_4 = "Este es el cuarto sector"
sectorId_5 = "Este es el quinto sector"
sectorId_6 = "Este es el quinto sector"
sectorId_7 = "Este es el quinto sector"
sectorId_8 = "Este es el quinto sector"

componente_1 = LanzaTorpedos
componente_2 = (Motor 5)
componente_3 = (Motor 10)
componente_4 = (Almacen [Comida, Oxigeno])
componente_5 = (Almacen [Comida, Oxigeno, Combustible])
componente_6 = (Almacen [Torpedo, Combustible, Combustible])

----------------------
type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
   deriving Show
data Manada = M Lobo
   deriving Show


--ej1
buenaCaza :: Manada -> Bool
   --Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías
buenaCaza (M lobos) = cantidadDeAlimento lobos > cantidadDeCrias lobos

cantidadDeAlimento :: Lobo -> Int
cantidadDeAlimento (Cria nom)                   = 0
cantidadDeAlimento (Explorador nom terrs l1 l2) = cantidadDeAlimento l1 + cantidadDeAlimento l2
cantidadDeAlimento (Cazador nom press l1 l2 l3) = length press + cantidadDeAlimento l1 + cantidadDeAlimento l2 + cantidadDeAlimento l3

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cria nom)                   = 1
cantidadDeCrias (Explorador nom terrs l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cazador nom press l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3

--ej2
elAlfa :: Manada -> (Nombre, Int)
   {-Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
   con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
   cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
   cero presas.-}

elAlfa (M lb) = esAlfa lb

esAlfa :: Lobo -> (Nombre, Int)
esAlfa (Cria nom)                   = (nom, 0)
esAlfa (Explorador nom terrs l1 l2) = tuplaConCantidadMayor (nom, 0) (tuplaConCantidadMayor (esAlfa l1) (esAlfa l2))
esAlfa (Cazador nom press l1 l2 l3) = (tuplaConCantidadMayor (nom, length press) (tuplaConCantidadMayor (esAlfa l1) (tuplaConCantidadMayor (esAlfa l2) (esAlfa l3))))


tuplaConCantidadMayor :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
tuplaConCantidadMayor  t1  t2  = if snd t1 > snd t2
                                 then t1
                                 else t2

--ej3
losQueExploraron :: Territorio -> Manada -> [Nombre]
   --Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
   --pasaron por dicho territorio.
losQueExploraron tr (M lb) = exploradoresDelTerritorio tr lb 

exploradoresDelTerritorio :: Territorio -> Lobo -> [Nombre]
exploradoresDelTerritorio tr (Cria nom)                   = []
exploradoresDelTerritorio tr (Cazador nom press l1 l2 l3) = exploradoresDelTerritorio tr l1 ++ exploradoresDelTerritorio tr l2 ++ exploradoresDelTerritorio tr l3
exploradoresDelTerritorio tr (Explorador nom terrs l1 l2) = if elem tr terrs
                                                            then nom : exploradoresDelTerritorio tr l1 ++ exploradoresDelTerritorio tr l2
                                                            else exploradoresDelTerritorio tr l1 ++ exploradoresDelTerritorio tr l2

--ej4









nombre_1  = "Asesino de bebes"
nombre_2  = "tanque de guerra"
nombre_3  = "gurí"
nombre_4  = "lobo malo"
nombre_5  = "rapunzel"
nombre_6  = "scarface"
nombre_7  = "el Peyu"
nombre_8  = "ABCDarius"
nombre_9 = "Astro boy"
nombre_10 = "King wolf"
nombre_11 = "La reina Malvada"
nombre_12 = "Chernabog"
nombre_13 = "Cristian castro"

presa_1  = "Alce"
presa_2  = "Bisonte"
presa_3  = "Benado"
presa_4  = "Antilope"
presa_5  = "Ciervo"
presa_6  = "Renoo"
presa_7  = "Zorro"
presa_8  = "Perro"
presa_9  = "Conejo"

trerritorio_1 = "territorio uno"
trerritorio_2 = "territorio dos"
trerritorio_3 = "territorio tres"
trerritorio_4 = "territorio cuatro"
trerritorio_5 = "territorio cinco"
trerritorio_6 = "territorio seis"
trerritorio_7 = "territorio siete"

lobo_1  = (Cazador nombre_1 [presa_1, presa_2, presa_3, presa_4] lobo_7 lobo_6 lobo_7)
lobo_2  = (Cazador nombre_5 [presa_5, presa_6, presa_7, presa_1, presa_2] lobo_1 lobo_5 lobo_13)

lobo_4  = (Explorador nombre_4 [trerritorio_1, trerritorio_5, trerritorio_3] lobo_12 lobo_7)
lobo_5  = (Explorador nombre_5 [trerritorio_1, trerritorio_5, trerritorio_3] lobo_8 lobo_9)
lobo_6  = (Explorador nombre_6 [trerritorio_6, trerritorio_7, trerritorio_3] lobo_10 lobo_11)

lobo_7  = (Cria nombre_7)
lobo_8  = (Cria nombre_8)
lobo_9  = (Cria nombre_9)
lobo_10 = (Cria nombre_10)
lobo_11 = (Cria nombre_11)
lobo_12 = (Cria nombre_12)
lobo_3  = (Cria nombre_3)
lobo_13 = (Cria nombre_13)

manada_1 = (M lobo_1)
