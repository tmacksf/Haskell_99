-- Extract a slice from a list.

slice :: [a]-> Int -> Int -> [a]
slice xs start end = [x | (x, i) <- zip xs [1..], (i >= start) && (i <= end)]

slice' :: [a] -> Int -> Int -> [a]
slice' xs s e= foldl (\acc (i, y) -> if (i >= s) && (i <= e) then acc ++ [y] else acc) [] (zip [1..] xs)
