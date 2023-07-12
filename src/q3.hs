-- Find the K'th element of a list.

kth :: [a] -> Integer -> a
kth [] y = error "out of bounds"
kth (x:_) 1 = x
kth (x:xs) y = kth xs (y-1)


-- TODO: Figure out how this works
kth' xs n 
  | length xs < n = error "out of bounds"
  | otherwise = last $ take n xs
