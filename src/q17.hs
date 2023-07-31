-- Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> [[a]]
split [x] _ = [[x]]
split x n 
  | length x <= n = [x]
  | otherwise = [get x n] ++ [release x n] 

get :: [a] -> Int -> [a] 
get x 0 = []
get [] _ = []
get (x:xs) n = x : (get xs (n-1))

release :: [a] -> Int -> [a] 
release x 0 = x
release (x:xs) n = release xs (n-1)

split' :: [a] -> Int -> [[a]]
split' xs n = [[x | (i, x) <- zip [1..] xs, i <= n],  
              [x | (i, x) <- zip [1..] xs, i > n]]
