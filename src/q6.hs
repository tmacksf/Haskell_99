-- Find out whether a list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome [] = True 
isPalindrome [x] = True
isPalindrome x 
  | length x `mod` 2 == 0 = 
      (reverse $ take (length x `div` 2) x) == (drop (length x`div` 2) x)
  | length x `mod` 2 == 1 = 
      (reverse $ take (length x `div` 2) x) == (drop (length x `div` 2 + 1) x)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' x = x == reverse x
