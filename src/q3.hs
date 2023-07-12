-- Find the K'th element of a list.

kth :: [a] -> Integer -> a
kth [] y = error "out of bounds"
kth (x:_) 0 = x
kth (x:xs) y = kth xs (y-1)
