-- Duplicate the elements of a list.

-- recursive answer
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = x : [x] 
dupli (x:xs) = x : [x] ++ dupli xs

dupli' :: [a] -> [a]
--dupli' x = foldl (\acc y-> acc ++ [y] ++ [y]) [] x 
dupli' x = foldr (\y acc-> [y] ++ [y] ++ acc) [] x 
--dupli' x = foldr (test) [] x 
