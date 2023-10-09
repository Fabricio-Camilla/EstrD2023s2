data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)   es O(log N) pq recorro una rama a la vez
belongsBST x     EmptyT      = False
belongsBST x (NodeT a ti td) = if (x==a) 
                                then True else if (x < a) 
                                    then belongsBST x td 
                                    else belongsBST x ti

insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
--Costo: O(log N)  es O(log N) pq recorro una rama a la vez
insertBST x     EmptyT      = NodeT x EmptyT EmptyT
insertBST x (NodeT a ti td) = if    (x==a)  then (NodeT x ti td) 
                            else if (x < a) then NodeT a (insertBST x ti) td
                                            else NodeT a ti (insertBST x td)



deleteBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST borra un elemento en el árbol.
--Precond: el tree a dado debe ser BST
--Costo: O(log N)  pq recorro una rama a la vez
deleteBST x      EmptyT     =  EmptyT
deleteBST x (NodeT a ti td) = if (x == a) then rearmarBST ti td 
                         else if (x < a)  then NodeT a (deleteBST x ti) td  
                                          else NodeT a  ti (deleteBST x td) 


rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
    --precon: Los tree a debe ser BST
    --Costo: O(log N)  pq recorro una rama a la vez
rearmarBST EmptyT  td  = td
rearmarBST   ti EmptyT = ti
rearmarBST   ti   td   =  let (a, ti') = splitMaxBST ti 
                           in NodeT a ti' td 

splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--Costo: O(log N)  pq recorro una rama a la vez
splitMinBST (NodeT a EmptyT td) = (a,td)
splitMinBST (NodeT a ti td)     = let (x, ti') = splitMinBST ti
                                    in (x, (NodeT x ti' td))

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--Costo: O(log N)    pq recorro una rama a la vez
splitMaxBST (NodeT a ti EmptyT) = (a,ti)
splitMaxBST (NodeT a ti td)     = let (x, td') = splitMaxBST td
                                    in (x, NodeT x ti td')

esBST :: Tree a -> Bool
--Propósito: indica si el árbol cumple con los invariantes de BST.
--Costo: O(N2)
esBST   EmptyT       = True
esBST (NodeT a ti td)= esMenorQueTodos a td &&  esMayorQueTodos a ti && esBST ti && esBST td

esMenorQueTodos :: a -> Tree a -> Bool
-- precond: debe estar orndenado
esMenorQueTodos x EmptyT          = True
esMenorQueTodos x (NodeT a ti td) = if x < a then esMenorQueTodos x ti  else False

esMayorQueTodos ::  a -> Tree a -> Bool
-- precond: debe estar orndenado
esMayorQueTodos x EmptyT          = True
esMayorQueTodos x (NodeT a ti td) = if x > a then esMayorQueTodos x td  else False

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
--Costo: O(log N)
elMaximoMenorA x    EmptyT      = Nothing
elMaximoMenorA x (NodeT a ti td)= if x < a  then maximoSiEsMenor x a td
                                            else elMaximoMenorA x ti

maximoSiEsMenor :: Ord a => a -> a -> Tree a -> Maybe a
--precond: y debe ser menor a x
maximoSiEsMenor x y     EmptyT     = Just y
maximoSiEsMenor x y (NodeT a ti td)= if x < a then maximoSiEsMenor x a td else Just y


maxBST :: Ord a => Tree a -> a 
--Precond: El tree dado no puede ser vacio.
maxBST (NodeT a ti td)=  a    
maxBST (NodeT a ti td)= maxBST td 



{-elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.
--Costo: O(log N)
balanceado :: Tree a -> Bool
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
--Costo: O(N2)-}