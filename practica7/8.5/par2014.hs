import PriorityQueue
data BitTree = Nil | Node Int BitTree BitTree
    deriving Show
data Bit = Off | On
    deriving Show
type Chain = [Bit]
    

{-
En un BitTree los enlaces a hijos izquierdos representan un bit en Off, mientras que los enlaces a hijos derechos
representan un bit en On. Esto significa que una cadena de bits est´a en el conjunto si en el ´arbol si existe una rama
que codifica la cadena. Adem´as en los nodos se almacena un entero que indica la cantidad de repeticiones de esa
cadena. La estructura mantiene un invariante de representaci´on que proh´ıbe que una hoja del ´arbol tenga asociado
una cantidad de repeticiones igual a cero
-}

             
contains :: BitTree -> Chain -> Int
--Que retorna la cantidad de repeticiones de una cadena en un arbol de bits (0 si la cadena no esta definida).
contains      Nil         _  = 0
contains (Node n ti td)   [] = n
contains (Node n ti td) (b:bs) =  if esOn b
                                        then contains td bs
                                        else contains ti bs

esOn :: Bit -> Bool
esOn On = True
esOn _ = False

insert :: BitTree -> Chain -> BitTree
--Que inserta una ocurrencia de la cadena en un arbol de bits
insert      Nil        ch  = construirArbol ch 
insert (Node n ti td)   [] = Node n ti td
insert (Node n ti td) (b:bs) = if esOn b
                                then Node n ti (insert td bs)
                                else Node n (insert ti bs) td

construirArbol :: Chain -> BitTree
construirArbol  []    = Node 1 Nil Nil
construirArbol (b:bs) = if esOn b 
                       then Node 0 Nil (construirArbol bs)
                       else Node 0 (construirArbol bs) Nil

remove :: BitTree -> Chain -> BitTree
--Que remueve una ocurrencia de una cadena en un arbol de bits
remove      Nil          ch  = Nil
remove       ar         []   = esArbolValido (podarArbol ar)
remove (Node n ti td) (b:bs) = if esOn b
                                 then esArbolValido (Node n ti (remove td bs))
                                 else esArbolValido (Node n (remove ti bs) td)

esArbolValido :: BitTree -> BitTree
esArbolValido        Nil      = Nil
esArbolValido (Node n Nil Nil)= if n == 0 then Nil else Node n Nil Nil
esArbolValido (Node n ti td)  = Node n ti td

podarArbol :: BitTree -> BitTree
podarArbol (Node n ti td) = if n > 1 
                           then (Node (n - 1) ti td)
                           else Nil 

elements :: BitTree -> [(Int,Chain)]
--Que devuelve una lista de las cadenas definidas en el ´arbol asociadas a sus repeticiones.
elements      Nil       = []
elements (Node n ti td) = let tu = agregarBitATupla Off (elements ti) ++ agregarBitATupla On (elements td)
                       in if n > 0
                          then (n,[]) : tu 
                          else  tu 

agregarBitATupla :: Bit  -> [(Int,Chain)] -> [(Int,Chain)]
agregarBitATupla b  []          = []
agregarBitATupla b  ((n,bs):bss) = (n,(b:bs)): agregarBitATupla b bss 


bt0 = Nil 
bt1 =            Node 0 (Node 2  bt0 bt0)     (Node 0 (Node 1 bt0 bt0) bt0)


data HTree = HLeaf Int | HBin Int HTree HTree
    deriving Show

weigth :: HTree -> Int
--Que retorna el peso de ´arbol de Huffman.
weigth  (HLeaf n)  = n
weigth (HBin n _ _)= n

weigthedSize :: HTree -> Int
--Que calcula el “tamanio ponderado” de un ´arbol de Huffman, que se calcula como la suma de los pesos de cada
--hoja multiplicada por la altura en la que se encuentra (considere que la raız esta a la altura 1 y cada hijo a un
--nivel de altura mas que el padre).
weigthedSize ht = obtenerWeightSize ht 1

obtenerWeightSize :: HTree -> Int -> Int
obtenerWeightSize (HLeaf n)  n2     = n * n2
obtenerWeightSize (HBin n ti td) n2 = (n2 * n) + obtenerWeightSize ti (n+1) + obtenerWeightSize td (n+1)

assemble :: PriortyQueue pq => pq HTree -> HTree
--Que genera un HTree a partir de una cola de prioridad de HTree (donde se considera que el arbol de menor peso
--tiene mayor prioridad en la cola). Nota: assemble trabaja combinando los dos nodos de menor peso (mayor
--prioridad) en un nodo binario con la suma de ambos y ası sucesivamente hasta quedarse con un unico nodo
--(que debe retornarse)
assemble pq = if isEmptyPQ pq
                then HLeaf 0
                else let (a,pq2) = dequeuePQ pq
                         (b,pq3) = dequeuePQ pq2
                     in assemble (enqueuePQ (armarHtree a b) pq3)

armarHtree :: HTree  -> HTree -> HTree
armarHtree h3 h = let newn = (weigth h + weigth h3)                 
                    in  (HBin newn h h3) 



