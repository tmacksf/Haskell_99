-- Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = [x] ++ compress (helper x xs)

helper :: (Eq a) => a -> [a] -> [a]
helper z [] = []
helper z [x]
  | z == x = []
  | z /= x = [x]
helper z (x:xs)
  | z == x = helper x xs 
  | z /= x = [x] ++ helper x xs 

