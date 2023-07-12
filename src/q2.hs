-- recursive solution

lastMinusOne :: [a] -> a
lastMinusOne [] = error "empty"
lastMinusOne [x] = x
lastMinusOne (x:y:[]) = x
lastMinusOne (x:xs) = lastMinusOne xs

lastMinusOne' = head . tail . reverse
