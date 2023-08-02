-- Remove the K'th element from a list.

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (last $ take n xs, take (n - 1) xs ++ drop n xs)

removeAt' n xs = (xs !! (n-1), [x | (x, i) <- zip xs [1..], n /= i])
