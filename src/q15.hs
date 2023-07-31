-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli x 0 = []
repli [x] n = x : repli [x] (n-1)
repli (x:xs) n = repli [x] n ++ repli xs n

repli' :: [a] -> Int -> [a]
repli' x n = foldr (\y acc -> (replicate n y) ++ acc) [] x
