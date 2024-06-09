main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 123456789 == 987654321 -- my test

rev :: Int -> Int
rev number = helper number 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper number result = helper (div number 10) (result * 10 + mod number 10)