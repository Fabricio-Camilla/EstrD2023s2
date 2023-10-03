
heapSport :: Ord a => [a] -> [a]
heapSport xs = pqToList (listaAPq xs)

pqToList :: Ord a => PriorityQueue -> [a]
pqToList pq = if isEmpetyPQ pq then [] else findMinPQ pq : pqToList (deleteMinPQ pq)

listaAPq :: Ord a => [a] -> PriorityQueue a -- O(insertPQ) + O(n) + O(emptyPQ)
listaAPq []      = emptyPQ 
listaAPq (x: xs) = insertPQ x (listaAPq xs)


agruparEq :: Eq k => [(k,v)] -> Map k [v]  --O(n) * O(assocM) + O(lookupM)
agruparEq []        = emptyM
agruparEq (k,v: xs) = case lookupM k (agruparEq xs) of  
                                  Just ys-> assocM k (v:ys) (agruparEq xs)
                                  Nothing-> assocM k [v] (agruparEq xs) -- lo llamo una vez por cada elemento de la lista


data MultiSet a = MS (Map a Int)

multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MS mp) = mapToList mp 

emptyMS :: MultiSet
emptyMS = MS emptyM   -- constante

addMS :: Ord => a -> MultiSet a -> MultiSet a  --O(lookupM) + O(assocM)
addMS x (MS mp) = case lookupM  x mp of
                  Just n -> MS (assocM x (n+1) mp)
                  Nothing -> MS (assocM x 1 mp )


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS a (MS mp) = case (lookupM a mp) of
                         Just n -> n 
                         Nothing -> 0



ocurrencias :: [a] -> [(a, Int)]
ocurrencias cs = multiSetToList(listToMs cs)

listToMs :: [a] -> MultiSet a 
listToMs []     = emptyMS
listToMs (x:xs) = addMS x (listToMs xs) 