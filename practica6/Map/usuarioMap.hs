import Map

agruparEq :: Eq k => [(k,v)] -> Map k v  --O(n) * O(assocM) + O(lookupM)
agruparEq []        = emptyM
agruparEq (k,v: xs) = case lookupM k (agruparEq xs) of  
                                  Just ys-> assocM k (v:ys) (agruparEq xs)
                                  Nothing-> assocM k [v] (agruparEq xs) -- lo llamo una vez por cada elemento de la lista
