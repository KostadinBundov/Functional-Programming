import Data.List

main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364
    print $ getPalindromes 109090 == 54547 -- my test


getPalindromes :: Int -> Int
getPalindromes n = last arr + head arr
 where
    arr = [x | x <- [2 .. n ], isPalindrome x && mod n x == 0]

isPalindrome :: Int -> Bool
isPalindrome number = number == (read $ reverse $ show $ number)
