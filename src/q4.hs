-- Find the number of elements in a list.

numElements :: [a] -> Integer 
numElements [] = 0
numElements [x] = 1
numElements (x:xs) = 1 + numElements xs
