main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 826
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 24
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 487
    print $ everyOther 4214 == 41
    print $ everyOther 123456789 == 2468 -- my test

everyOther :: Int -> Int
everyOther number = helper number 0 1 0
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper 0 _ _ result = result
    helper number index multiplier result
     | mod index 2 /= 0 = helper (div number 10) (index + 1) (multiplier * 10) ((mod number 10) * multiplier + result)
     | otherwise = helper (div number 10) (index + 1) multiplier result