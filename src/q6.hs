-- Find out whether a list is a palindrome.

isPalindrome :: [Int] -> Bool 
isPalindrome [] = False 
isPalindrome [x] = True
isPalindrome x 
  | length x `mod` 2 == 0 = 
      (reverse $ take (length x`div` 2) x) == (drop (length x`div` 2) x)
  | length x `mod` 2 == 1 = 
      (reverse $ take (length x `div` 2) x) == (drop (length x `div` 2 + 1) x)
