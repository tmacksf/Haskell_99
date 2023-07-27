-- Modified run-length encoding.

data Output a = Single a | Multiple Int a deriving (Show)

addOutput :: Output a -> Output a
addOutput (Multiple x a) = (Multiple (x+1) a) 
addOutput (Single a) = (Multiple 2 a)

encode :: (Eq a) => [a] -> [Output a] 
encode [] = [] 
encode [x] = [Single x]
encode (x:xs) = (convert (1 + (length $ takeWhile (==x) xs )) x) : encode (dropWhile (==x) xs) where 
    convert :: Int -> a -> Output a
    convert 1 a = (Single a)
    convert x a = (Multiple x a)
