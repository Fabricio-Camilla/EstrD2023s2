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


