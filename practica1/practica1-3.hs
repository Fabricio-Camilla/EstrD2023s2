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