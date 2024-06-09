import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ sumSpecialPrimes 2 3 == 16 -- my test

isPrime :: Int -> Bool
isPrime number = number > 1 && not (any (\x -> mod number x == 0) [2..number-1])

containsDigit :: Int -> Int -> Bool
containsDigit number digit = elem (intToDigit digit) (show number)

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes count digit =  sum $ take count $ filter (\ x -> isPrime x && containsDigit x digit)[2 ..]