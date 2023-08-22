sucesor :: Int -> Int
sucesor n = n + 1

sumar :: Int -> Int -> Int
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = max n m 

{- 2 ejemplos
maxDelPar (divisionYResto (sumar 5 5) (sucesor 0))
maxDelPar (divisionYResto (sumar 50 50) (sucesor 9))
maxDelPar (divisionYResto (sumar 900 100) (sucesor 99))
maxDelPar (divisionYResto (sumar 10 10) (sucesor 1))
maxDelPar (divisionYResto (sumar 25 25) (sucesor 4))
-}

data Dir = Norte | Este | Sur | Oeste
    deriving Show
    
opuesto :: Dir -> Dir 
opuesto Norte = Sur 
opuesto Este  = Oeste 
opuesto Sur   = Norte 
opuesto Oeste = Este 

iguales :: Dir -> Dir -> Bool
iguales Norte Norte  = True
iguales Este Este    = True 
iguales Sur Sur      = True
iguales Oeste Oeste  = True
iguales _     _      = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = Norte

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
 deriving Show
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM  Lunes     = False
empiezaConM  Martes    = True
empiezaConM  Miercoles = True
empiezaConM  Jueves    = False
empiezaConM  Viernes   = False
empiezaConM  Sabado    = False
empiezaConM  Domingo   = False


vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes  Lunes     = True
vieneDespues Miercoles Lunes   = True
vieneDespues Miercoles Martes  =True
vieneDespues Jueves  Miercoles = True
vieneDespues Jueves  Martes = True
vieneDespues Jueves  Lunes = True
vieneDespues Viernes Jueves    = True
vieneDespues Viernes Miercoles    = True
vieneDespues Viernes Martes   = True
vieneDespues Viernes Lunes    = True
vieneDespues Sabado  Viernes   = True
vieneDespues Domingo Sabado    = True
vieneDespues Lunes   Domingo   = True
vieneDespues _        _        = False


estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Martes            = True
estaEnElMedio Miercoles         = True
estaEnElMedio Jueves            = True
estaEnElMedio Viernes           = True
estaEnElMedio Sabado            = True
estaEnElMedio primeroYUltimoDia = False

negar :: Bool -> Bool
negar True  = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica  _    _    = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien  _     _  = False

oBien :: Bool -> Bool -> Bool
oBien True  _     = True
oBien  _   True   = True
oBien False False = False

data Persona = P Int  String 
            -- edad    nombre
    deriving Show

nombre :: Persona -> String
nombre (P _ n) = n

edad :: Persona -> Int
edad (P e _) = e 

crecer :: Persona -> Persona
crecer (P e n) =  (P(e + 1) n)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (P e _ ) = P e n

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2


laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor  p1 p2  = if (esMayorQueLaOtra p1 p2)
                       then p1
                       else p2

yo= P 30 "Fabri"
yo2= P 30 "fa"

data TipoPokemon = Agua | Fuego | Planta 
    deriving Show

data Pokemon = Po TipoPokemon Int
    deriving Show
                            --porcentaje energia

data Entrenador = E String Pokemon Pokemon
                   --nombre

superaA :: Pokemon -> Pokemon -> Bool
superaA p1   p2    = esDeTipo(tipoDePokemon p1) (tipoDePokemon p2)


esDeTipo :: TipoPokemon -> TipoPokemon -> Bool
esDeTipo Agua Fuego   = True
esDeTipo Fuego Planta = True
esDeTipo Planta Agua  = True
esDeTipo _        _   = False

tipoDePokemon :: Pokemon -> TipoPokemon
tipoDePokemon (Po t _) = t

sumar :: Int -> Int -> Int
sumar n m = n + m

cantidadDePokemon :: TipoPokemon -> Entrenador -> Int
cantidadDePokemon t  (E _ p1 p2)  = sumar (cantidadSiEsDeTipo (t) (tipoDePokemon p1)) (cantidadSiEsDeTipo (t) (tipoDePokemon p2))


cantidadSiEsDeTipo :: TipoPokemon ->  TipoPokemon -> Int
cantidadSiEsDeTipo t1 t2  = 1
cantidadSiEsDeTipo t1 _   = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ p1 p2), (E _ p3 p4)) = [p1, p2, p3, p4]



entrenador1 = E "sho" poke1 poke2
entrenador = E "el" poke2 poke1
poke1 = Po Agua 30
poke2 = Po Agua 20

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete  x = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--son fuciones polimorficas por que importa la estructura, pero no los datos específcos

estaVacia :: [a] -> Bool
estaVacia []  = True
estaVacia [x] = False

elPrimero :: [a] -> a
elPrimero (x: _ ) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : x) = x

splitHead :: [a] -> (a,[a])
splitHead x = ((elPrimero x),( sinElPrimero x))