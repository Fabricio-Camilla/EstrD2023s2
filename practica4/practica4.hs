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
caminoDeLaRamaMasLarga (Fin cof)               =   
caminoDeLaRamaMasLarga (Bifurcacion cof m1 m2) = if length caminoDeLaRamaMasLarga m1 > length caminoDeLaRamaMasLarga m2
                                                 then Izq : caminoDeLaRamaMasLarga m1
                                                 else Der : caminoDeLaRamaMasLarga m2



--ej5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cof)              = objetosDelCofre cof
tesorosPorNivel (Bifurcacion cf m1 m2) = tesorosDe cf : (unificar (tesorosPorNivel m1) (tesorosPorNivel m2))
   
unificar :: [[Objeto]] -> [[Objeto]] ->[[Obejto]]   
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
todosLosCaminos (Bifurcacion cof m1 m2) = agregarATodos (Izq  todosLosCaminos m1) ++ agregarATodos (Der  todosLosCaminos m2)

agregarATodos :: Dir -> [[Dir]] -> [[Dir]]
agregarATodos dir    []    = []
agregarATodos dir (dr:dss) = (dir : dr ) : agregarATodos dss

cofre1 = (Cofre [Chatarra,Tesoro])
cofre2 = (Cofre [Chatarra, Chatarra ])
cofre3 = (Cofre [Chatarra, Chatarra, Chatarra,Tesoro])
cofre4 = (Cofre [Tesoro, Tesoro])
cofre5 = (Cofre [Tesoro, Chatarra, Tesoro])

mapa1 = (Bifurcacion cofre2 (Fin cofre1) (Fin cofre3))
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]

data Barril = Comida | Oxigeno | Torpedo | Combustible

data Sector = S SectorId [Componente] [Tripulante]

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Nave = N (Tree Sector)

--nave=N(NodeT S("sector1",[LanzaTorpedos], ["sho"]) S("sector2", [LanzaTorpedos], ["sho2"]) )

{---ej1
sectores :: Nave -> [SectorId]
--Propósito: Devuelve todos los sectores de la nave.
sectores EmptyT              =  []
sectores (NodeT s sizq sder) =  idDelSector s : sectores sizq ++ sectores sder
-}

idDelSector :: Sector -> SectorId
idDelSector (S sid comps trip) = sid


agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector comps sId (N tr) = N extenderSector comps sId tr


extenderSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
extenderSector comps sId EmptyT           = EmptyT
extenderSector comps sId (NodeT s t1 t2)  =  if sId == idDelSector s 
                                             then (NodeT (agregarLe comps s) t1 t2 )
                                             else (NodeT s (extenderSector comps sId t1) (extenderSector comps sId t2 ))

agregarLe :: [Componente] -> Sector -> [Componente]
agregarLe  []          s            =  []
agregarLe  comps  (S id comPs trip) =  comps : comPs
