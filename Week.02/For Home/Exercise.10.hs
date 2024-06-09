main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11
    print $ countPalindromes 5 23 == 6 -- my test -- 6 7 8 9 11 22

reverseNumber :: Int -> Int
reverseNumber n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)

countPalindromes :: Int -> Int -> Int
countPalindromes start end
 | start <= end = helper (start + 1) 0 end
 | otherwise =  helper (end + 1) 0 start
  where
    helper :: Int -> Int -> Int -> Int
    helper currNumber count end
     | currNumber > end = count
     | currNumber == reverseNumber currNumber = helper (currNumber + 1) (count + 1) end
     | otherwise = helper (currNumber + 1) count end