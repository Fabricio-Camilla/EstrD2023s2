data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
   deriving Show

--ej1
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia      = 0
nroBolitas c (Bolita c2 cel) =  unoSiCeroSiNo(esMismoColor c c2) + nroBolitas c cel

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _      _  = False

unoSiCeroSiNo :: Bool -> Int
unoSiCeroSiNo True = 1
unoSiCeroSiNo   _  = 0

celda= (Bolita Rojo CeldaVacia)

--ej2
poner :: Color -> Celda -> Celda
poner c cd = Bolita c cd

--ej3
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c  (Bolita c2 cd) = if esMismoColor c c2
                     then cd
                     else  Bolita c2 (sacar c cd) 
--ej4
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cd  = cd
ponerN n c cd  = Bolita c (ponerN(n-1) c cd)




--ej1
--Indica si hay un cofre con un tesoro en el camino

hayTesoro :: Camino -> Bool
hayTesoro Fin    = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre obs c) = tieneTesoro obs || hayTesoro c 

tieneTesoro :: [Objeto] -> Bool
tieneTesoro []     = False
tieneTesoro (x:xs) =  esTesoro x ||  tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro   = True
esTesoro   _      = False

data Objeto = Cacharro | Tesoro
  deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving Show
--ej2
{-Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro-}

pasosHastaTesoro :: Camino -> Int
    --PRECOND: tiene que haber al menos un tesoro
pasosHastaTesoro Fin           = error "Hay al menos un tesoro"
pasosHastaTesoro (Nada c)      = 1 + pasosHastaTesoro c 
pasosHastaTesoro (Cofre obs c) = if  tieneTesoro obs
                                 then 0
                                 else 1 + pasosHastaTesoro c 

--ej3
{-indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.-}
camino =  Cofre [Cacharro, Cacharro,Tesoro,Tesoro,Cacharro](Nada(Nada Fin))

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0  c            = hayTerosoroAhora c 
hayTesoroEn n Fin           = False
hayTesoroEn n (Nada c)      = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre obs c) = hayTesoroEn (n-1) c

hayTerosoroAhora :: Camino -> Bool
hayTerosoroAhora (Cofre obs c) = tieneTesoro obs
hayTerosoroAhora         _     = False

--ej4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0    _            = True
alMenosNTesoros _    Fin          = False
alMenosNTesoros n  (Nada c)       = alMenosNTesoros n c
alMenosNTesoros n  (Cofre obs c)  = alMenosNTesoros (n - (cantTesoros obs )) c
  
  
-- n <= cantidadTesorosEn c

cantidadTesorosEn :: Camino -> Int
cantidadTesorosEn Fin           =  0
cantidadTesorosEn (Nada c)      = cantidadTesorosEn c
cantidadTesorosEn (Cofre obs c) = cantTesoros obs  + cantidadTesorosEn c

cantTesoros :: [Objeto] -> Int
cantTesoros   []   =  0
cantTesoros (x:xs) = unoSiCeroSiNo (esTesoro x) +  cantTesoros xs 

--ej5
cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre n m Fin          = 0
cantTesorosEntre n m (Nada c)     = cantTesorosEntre n m c
cantTesorosEntre n m (Cofre obs c)= cantTesoros(tomarEntre n m obs) + cantTesorosEntre n m c

tomarEntre :: Int -> Int -> [Objeto] -> [Objeto]
tomarEntre n m xs = tomarDesde n (tomarHasta (m+1) xs)

tomarHasta :: Int -> [Objeto] -> [Objeto]
tomarHasta 0 _      = []
tomarHasta _ []     = [] 
tomarHasta n (x:xs) = x : tomarHasta (n-1) xs

tomarDesde :: Int -> [Objeto] -> [Objeto]
-- PRECOND: i >= 0
tomarDesde 0 ys     = ys
tomarDesde n []     = []
tomarDesde n (_:xs) = tomarDesde (n-1) xs




data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
     deriving Show

--arbol1 :: Tree Int


--ej1
sumarT :: Tree Int -> Int
sumarT EmptyT    = 0
sumarT (NodeT a t1 t2) = a + sumarT t1 + sumarT t2 

--ej2
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT a t1 t2) = 1 + sizeT t1 + sizeT t2

--ej3
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT a t1 t2) = NodeT (a*2) (mapDobleT t1)  (mapDobleT t2)


--ej4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT           = False
perteneceT e (NodeT x t1 t2)  = e==x || perteneceT e t1 || perteneceT e t2

--ej5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e  EmptyT         = 0
aparicionesT e (NodeT x t1 t2) = unoSiCeroSiNo (e==x) + aparicionesT e t1 + aparicionesT e t2

--ej6
leaves :: Tree a -> [a]
leaves EmptyT           = []
leaves (NodeT a t1 t2)  = if esVacia t1 && esVacia t2 then [a] else leaves t1 ++ leaves t2

esVacia :: Tree a -> Bool
esVacia EmptyT = True
esVacia   _    = False

--ej7
--preg--
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT a t1 t2) = 1 + max (heightT t1)  (heightT t2)

--ej8
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          =  EmptyT
mirrorT (NodeT a t1 t2) =  NodeT a (mirrorT t2) (mirrorT t1)

--ej9
toList :: Tree a -> [a]
toList EmptyT         = []
toList (NodeT a t1 t2)= (toList t1) ++ [a] ++ (toList t2)

--ej10
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT         = [] 
levelN n (NodeT x t1 t2)= if n==0 
                          then [x]  
                          else levelN (n-1) t1 ++ levelN (n-1) t2

--ej11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : unificarNivelesDe (listPerLevel t1) (listPerLevel t2)

unificarNivelesDe :: [[a]] -> [[a]] ->[[a]]
unificarNivelesDe   []     xs  = xs
unificarNivelesDe    xs    []  = xs
unificarNivelesDe (y:ys) (x:xs)= (y ++ x) : unificarNivelesDe ys xs

--ej12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = if (sizeT ti) > ( sizeT td) 
                               then x : ramaMasLarga ti 
                               else x : ramaMasLarga td


--ej13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x ti td)=  agregarATodas x  (todosLosCaminos ti) ++ (todosLosCaminos td) 

agregarATodas :: a -> [[a]] ->[[a]]
agregarATodas a []     = [a] : []
agregarATodas a (xs:xss) = (a:xs) : agregarATodas a xss



arbol1 = NodeT 1 (NodeT 2 (NodeT 4 EmptyT EmptyT)  (NodeT 1 EmptyT(NodeT 4 EmptyT(NodeT 4 EmptyT(NodeT 4 EmptyT EmptyT)))))
                 (NodeT 3 (NodeT 4 EmptyT EmptyT) (NodeT 5 EmptyT EmptyT))


data ExpA = Valor Int| Sum ExpA ExpA| Prod ExpA ExpA| Neg ExpA
   deriving Show

miExpresion :: ExpA
miExpresion = (Sum (Valor 0) (Neg (Neg (Sum (Valor 5) (Valor 0)))))

--ej1
eval :: ExpA ->  Int
eval (Valor x)    = x
eval (Sum x1 x2)  = (eval x1) + (eval x2)
eval (Prod x1 x2) = (eval x1) * (eval x2)
eval (Neg  x)    = -eval x

--ej2

simplificar :: ExpA -> ExpA
simplificar (Valor n)       = Valor n
simplificar (Sum ex1 ex2)   = simpSuma (simplificar ex1) (simplificar ex2)
simplificar (Prod ex1 ex2)  = simpProd (simplificar ex1) (simplificar ex2)
simplificar (Neg ex1)       = simpNeg  (simplificar ex1)


simpSuma :: ExpA -> ExpA -> ExpA 
simpSuma  ex1       (Valor 0) = ex1
simpSuma (Valor 0)    ex1     = ex1
simpSuma ex1          ex2     = Sum ex1 ex2

simpProd :: ExpA -> ExpA -> ExpA
simpProd (Valor 1)   ex2       = ex2
simpProd ex1        (Valor 1)  = ex1 
simpProd (Valor 0)    ex2      = Valor 0 
simpProd ex1        (Valor 0)  = Valor 0
simpProd ex1          ex2      = Prod ex1 ex2

simpNeg :: ExpA -> ExpA 
simpNeg (Neg (Neg (ex1))) = ex1
simpNeg     ex1           = ex1


--a) 0 + x = x + 0 = x
--b) 0 * x = x * 0 = 0
--c) 1 * x = x * 1 = x
--d) - (- x) = x


