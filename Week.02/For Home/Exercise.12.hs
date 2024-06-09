main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98
    print $ findSum 0 3 5 == 159 -- my test

sumPowerOfTwo :: Int -> Int
sumPowerOfTwo 0 = 1
sumPowerOfTwo k = 2^k + sumPowerOfTwo (k - 1)

findSum :: Int -> Int -> Int -> Int
findSum a b n = helper (n-3) 0
 where
    helper :: Int -> Int -> Int
    helper curr result
     | curr >= n = result
     | otherwise = helper (curr + 1) (result + a + b * sumPowerOfTwo curr)