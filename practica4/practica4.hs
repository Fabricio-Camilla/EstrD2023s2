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