sucesor :: Int -> Int
sucesor n = n + 1



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
opuesto Norte = Este
opuesto Este  = Sur 
opuesto Sur   = Norte 
 

iguales :: Dir -> Dir -> Bool
iguales Norte Norte  = True
iguales Este Este    = True 
iguales Sur Sur      = True
iguales Oeste Oeste  = True
iguales _     _      = False

siguiente :: Dir -> Dir
--PRECOND: Oeste no se puede utilizar
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Norte
siguiente Oeste = error "La direccion Oeste no esta contemplada"


data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
 deriving Show
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM  Martes    = True
empiezaConM  Miercoles = True
empiezaConM  _         = False


vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDia d1 > numeroDia d2

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5 
numeroDia Sabado    = 6
numeroDia Domingo   = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = numeroDia d >= 2 && numeroDia d <= 6


negar :: Bool -> Bool
negar True  = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True  b = b
implica False _ = True


yTambien :: Bool -> Bool -> Bool
yTambien True b1 = b1
yTambien _  _    = False


oBien :: Bool -> Bool -> Bool
oBien True _  = True
oBien  _   b1 = b1



data Persona = P Int  String 
            -- edad    nombre
    deriving Show

nombre :: Persona -> String
nombre (P _ n) = n

edad :: Persona -> Int
edad (P e _) = e 

crecer :: Persona -> Persona
crecer (P e n) =  P (e + 1) n

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
superaA p1   p2    = esDeTipoSuperador(tipoDePokemon p1) (tipoDePokemon p2)


esDeTipoSuperador :: TipoPokemon -> TipoPokemon -> Bool
esDeTipoSuperador Agua   Fuego   = True
esDeTipoSuperador Fuego  Planta  = True
esDeTipoSuperador Planta Agua    = True
esDeTipoSuperador _        _     = False

tipoDePokemon :: Pokemon -> TipoPokemon
tipoDePokemon (Po t _) = t

sumar :: Int -> Int -> Int
sumar n m = n + m

cantidadDePokemon :: TipoPokemon -> Entrenador -> Int
cantidadDePokemon t  (E _ p1 p2)  = unoSiEsDelMismoTipo t (tipoDePokemon p1) + unoSiEsDelMismoTipo t (tipoDePokemon p2)

unoSiEsDelMismoTipo :: TipoPokemon ->  TipoPokemon -> Int
unoSiEsDelMismoTipo t1 t2 = if(esDelMismoTipo t1 t2)
                            then 1
                            else 0
                            
esDelMismoTipo ::  TipoPokemon -> TipoPokemon -> Bool
esDelMismoTipo Agua   Agua   = True
esDelMismoTipo Fuego  Fuego  = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo  _       _    = False



juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = (pokemonsDe e1) ++ (pokemonsDe e2)


pokemonsDe :: Entrenador -> [Pokemon] 
pokemonsDe (E _ p1 p2) = [p1,p2]


entrenador1 = E "sho" poke1 poke2
entrenador = E "el" poke2 poke1
poke1 = Po Fuego 30
poke2 = Po Fuego 20

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete  x = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--son fuciones polimorficas por que importa la estructura, pero no los datos específcos

estaVacia :: [a] -> Bool
estaVacia []  = True
estaVacia (_:_) = False

elPrimero :: [a] -> a
--PRECOND: La lista no puede estar vacia
elPrimero    []   = error "No puede ser una lista vacia"
elPrimero (x: _ ) = x

sinElPrimero :: [a] -> [a]
--PRECOND: La lista no puede estar vacia.
sinElPrimero   []     = error "No puede ser una lista vacia"
sinElPrimero (_ : xs) = xs


splitHead :: [a] -> (a,[a])
--PRECOND: La lista no puede estar vacia
splitHead x  = (elPrimero x,sinElPrimero x)
splitHead [] = error "No puede ser una lista vacia"