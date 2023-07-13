-- Find the number of elements in a list.

numElements :: [a] -> Integer 
numElements [] = 0
numElements (x:xs) = 1 + numElements xs

numElements' :: [a] -> Int 
numElements' = sum . map (\_->1)
