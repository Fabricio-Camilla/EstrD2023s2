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

