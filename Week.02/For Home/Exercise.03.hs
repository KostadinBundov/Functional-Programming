main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5
    print $ sumPrimeDivs 18 == 5
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53
    print $ sumPrimeDivs 20 == 7 -- my test

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
    helper :: Int -> Bool
    helper divisor
     | divisor >= n = True
     | mod n divisor == 0 = False
     | otherwise = helper $ divisor + 1


sumPrimeDivs :: Int -> Int
sumPrimeDivs number = helper 2 number 0
 where
    helper :: Int -> Int -> Int -> Int
    helper iter number result
     | iter > number = result
     | mod number iter == 0 && isPrime iter = helper (iter + 1) number (result + iter)
     | otherwise = helper (iter + 1) number result
