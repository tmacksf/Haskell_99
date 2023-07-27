-- Run-length encoding of a list (direct solution).
-- Not really sure about this one. Come back to in the future
-- TODO: Figure this out

data Output a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Output a] 
encodeDirect [] = []
encodeDirect (x:xs) = encodeHelper 1 x xs

-- The solution shown
encodeHelper :: (Eq a) => Int -> a -> [a] -> [Output a]
encodeHelper n y [] = [singleEncoder n y] 
encodeHelper n y (x:xs)
  | y == x = encodeHelper (n + 1) y xs
  | otherwise = singleEncoder n y : (encodeHelper 1 x xs) 

singleEncoder :: Int -> a -> (Output a)
singleEncoder 1 a = (Single a)
singleEncoder x a = (Multiple x a)

