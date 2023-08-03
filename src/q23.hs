-- Extract a given number of randomly selected elements from a list.
import System.Random 
import System.IO.Unsafe

rnd_select :: [a] -> Int -> [a]
rnd_select xs 1 = [xs !! randomNum (length xs - 1)]
rnd_select xs i = [xs !! randomNum (length xs - 1)] ++ rnd_select xs (i-1) 

randomNum :: Int -> Int
randomNum x = unsafePerformIO (getStdRandom (randomR (0, x)))
