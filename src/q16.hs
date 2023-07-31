-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery x n = foldl (\acc y-> if (fst y) `mod` n /= 0 then acc ++ [snd y] else acc) [] (zip [1..] x)

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = [x | (i, x) <- zip [1..] xs, i `mod` n /= 0] 
