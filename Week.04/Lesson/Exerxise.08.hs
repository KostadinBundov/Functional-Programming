import Data.Char

main :: IO()
main = do
    print $ rev 123 == 321

    print $ fact 5 == 120

    print $ isPrime 5 == True
    print $ isPrime 1 == False
    print $ isPrime 6 == False
    print $ isPrime 11 == True
    print $ isPrime 13 == True

    -- print $ sumDig 142500 == 12

    -- print $ sumDivs 161 == 192

rev :: Int -> Int
rev = read . reverse . show

fact :: Int -> Int
fact number = product [1 .. number]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime number = not $ any (\x -> mod number x == 0) [2 .. number - 1] 