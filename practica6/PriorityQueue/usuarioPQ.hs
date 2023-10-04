import PriorityQueue


heapSport :: Ord a => [a] -> [a]
heapSport xs = pqToList (listaAPq xs)

pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq = if isEmpetyPQ pq then [] else findMinPQ pq : pqToList (deleteMinPQ pq)

listaAPq :: Ord a => [a] -> PriorityQueue a -- O(insertPQ) + O(n) + O(emptyPQ)
listaAPq []      = emptyPQ 
listaAPq (x: xs) = insertPQ x (listaAPq xs)