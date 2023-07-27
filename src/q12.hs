-- Decode a run-length encoded list.

data Output a = Single a | Multiple Int a deriving (Show)

getNum :: Output a -> Int
getNum (Single a) = 1
getNum (Multiple x a) = x

getVal :: Output a -> a
getVal (Single a) = a
getVal (Multiple x a) = a  

dup :: Int -> a -> [a]
dup 0 a = [] 
dup x a = a : dup (x-1) a 

-- decodeModified xs = [dup (getNum x) (getVal x) | x <- xs ]

decodeModified :: [Output a] -> [a]
decodeModified [] = [] 
decodeModified [x] = dup (getNum x) (getVal x) 
decodeModified (x:xs) = dup (getNum x) (getVal x) ++ decodeModified xs 

-- Using builtins
decodeModified' :: [Output a] -> [a]
decodeModified' = concatMap decodeBruh 

decodeBruh :: Output a -> [a]
decodeBruh (Single v) = [v] 
decodeBruh (Multiple x v) = replicate x v
