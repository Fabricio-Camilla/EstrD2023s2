import MultiSet

ocurrencias :: [a] -> [(a, Int)]
ocurrencias cs = multiSetToList(listToMs cs)

listToMs :: [a] -> MultiSet a 
listToMs []     = emptyMS
listToMs (x:xs) = addMS x (listToMs xs) 