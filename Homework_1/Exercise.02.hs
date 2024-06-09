main :: IO()
main = do
    print $ maxPersistenceMinSum 273 392 == 355
    print $ maxPersistenceMinSum 1000 2000 == 1679
    print $ maxPersistenceMinSum 55 105 == 77
    print $ maxPersistenceMinSum 195 756 == 679
    print $ maxPersistenceMinSum 2 85 == 77

getMultiplicationDigits :: Int -> Int
getMultiplicationDigits number
 | number < 10 = number
 | otherwise = mod number 10 * getMultiplicationDigits (div number 10)

getPersistenceNumber :: Int -> Int
getPersistenceNumber number
 | number < 10 = 0
 | otherwise = 1 + getPersistenceNumber (getMultiplicationDigits number)

getSumDigits :: Int -> Int
getSumDigits number
 | number < 10 = number
 | otherwise = (mod number 10) + getSumDigits (div number 10)

maxPersistenceMinSum :: Int -> Int -> Int
maxPersistenceMinSum start end = helper start 0 start
 where
  helper :: Int -> Int -> Int -> Int
  helper curr maxPersistant result
   | curr > end = result
   | (getPersistenceNumber curr) > maxPersistant = helper (curr + 1) (getPersistenceNumber curr) curr
   | (getPersistenceNumber curr) == maxPersistant && currSum < maxSum = helper (curr + 1) maxPersistant curr
   | otherwise = helper (curr + 1) maxPersistant result
    where
        currSum = getSumDigits curr
        maxSum = getSumDigits result
