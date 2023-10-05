
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