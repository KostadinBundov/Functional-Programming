main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

getSumDigits :: Int -> Int
getSumDigits 0 = 0
getSumDigits n = mod n 10 + (getSumDigits $ div n 10)


sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = evaluate (min start finish) (max start finish)
 where
    evaluate :: Int -> Int -> Int
    evaluate realStart realFinish
     | realStart > realFinish = 0
     | mod (getSumDigits realStart) k == 0 = realStart + evaluate (realStart + 1) realFinish
     | otherwise = evaluate (realStart + 1) realFinish