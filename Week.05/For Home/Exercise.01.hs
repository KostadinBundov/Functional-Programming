import Data.Char
import Data.List

main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 20 == [7,17] -- my test

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC start end = [x | x <- [min start end .. max start end], isPrime x && contains x 7 ]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF start end = filter (\ x -> isPrime x && contains x 7) [min start end .. max start end]

isPrime :: Int -> Bool
isPrime number = number > 1 && (not $ any (\x -> mod number x == 0) [2..number-1])

contains :: Int -> Int -> Bool
contains number digit = elem (intToDigit digit) $ show number