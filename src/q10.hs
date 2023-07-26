-- Run-length encoding of a list.
-- (Huffman encoding)

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode (x:xs) = [(1 + (length $ takeWhile (== x) xs), x)] ++ encode (dropWhile (== x) xs)

