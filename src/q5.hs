-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = [] 
myReverse (x:xs) = myReverse xs ++ [x] 

append accum x = accum ++ x

prepend' accum x = x ++ accum

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' x = foldl (\acc y -> y : acc) [] x
