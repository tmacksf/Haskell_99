-- Find the last element of a list.
-- not using the last function

-- Done with recursion
last' :: [a] -> a
last' [] = error "no last in empty list"
last' (x:[]) = x
last' (x:xs) = last (xs) 

last'' = head . reverse
