-- Pack consecutive duplicates of list elements into sublists.

-- Very round about way of doing this but it works
pack :: (Eq a) => [a] -> [[a]]
pack [] = [] 
pack [x] = [[x]]
pack (x:xs) = [[x] ++ takeWhile (== x) xs] ++ pack (drop (bruh xs x) xs)

bruh :: (Eq a) => [a] -> a -> Int
bruh [] y = 0
bruh [x] y = if x == y then 1 else 0 
bruh (x:xs) y
  | x == y = 1 + bruh xs y
  | otherwise = 0 

pack'' :: (Eq a) => [a] -> [[a]]
pack'' [] = [] 
pack'' [x] = [[x]]
pack'' (x:xs) = [[x] ++ takeWhile (== x) xs] ++ pack'' (dropWhile (==x) xs)


pack' :: (Eq a) => [a] -> [[a]]
pack' = foldr fn []
    where fn y [] = [[y]]
          fn y (x:xs) = if y == head x then ((y:x):xs) else ([y]:x:xs) 

