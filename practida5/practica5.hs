import Set


head' :: [a] -> a  --costo constante
head' (x:xs) = x 

sumar :: Int -> Int   --costo constante
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 

factorial :: Int -> Int    --costo lineal
factorial 0 = 1
factorial n = n * factorial (n-1)  

longitud :: [a] -> Int      --costo lineal
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 

factoriales :: [Int] -> [Int]     --costo cuadratica
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs 

pertenece :: Eq a => a -> [a] -> Bool   --costo lineal
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs 

sinRepetidos :: Eq a => [a] -> [a]  --costo cuadratica
sinRepetidos [] = []
sinRepetidos (x:xs) =if pertenece x xs
                then sinRepetidos xs
                else x : sinRepetidos xs
   

-- equivalente a (++)
append :: [a] -> [a] -> [a]  --costo lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys  

concatenar :: [String] -> String     --costo lineal
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs 

takeN :: Int -> [a] -> [a]    --costo lineal
takeN 0 xs = []
takeN n [] = []  
takeN n (x:xs) = x : takeN (n-1) xs 

dropN :: Int -> [a] -> [a]   --costo lineal
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs   

partir :: Int -> [a] -> ([a], [a])     --costo cuadratica
partir n xs = (takeN n xs, dropN n xs)  

minimo :: Ord a => [a] -> a     --costo lineal
minimo [x] = x
minimo (x:xs) = min x (minimo xs)  

sacar :: Eq a => a -> [a] -> [a]  --costo lineal
sacar n [] = []
sacar n (x:xs) =if n == x
                then xs
                else x : sacar n xs  

ordenar :: Ord a => [a] -> [a]  --costo cuadratica
ordenar [] = []
orderar xs = let m = minimo xs
             in m : ordenar (sacar m xs) 


------------------------------------


ff :: Set Int
ff = losQuePertenecen [1] (addS 1 emptyS)

{-

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.


unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.-}