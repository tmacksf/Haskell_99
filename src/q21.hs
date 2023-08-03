-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a] 
insertAt x xs i = take (i-1) xs ++ [x] ++ (drop (i-1) xs)

insertAt' :: a -> [a] -> Int -> [a] 
insertAt' y xs 1 = y:xs
insertAt' y (x:xs) i = x : insertAt y xs (i-1) 
