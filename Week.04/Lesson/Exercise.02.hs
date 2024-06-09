main :: IO()
main = do
    print $ myLengthRecNonPM [] == 0
    print $ myLengthRecNonPM [1, 2, 3] == 3

    print $ myLengthRecPM [] == 0
    print $ myLengthRecPM [1, 2, 3] == 3

    print $ myLengthFunc [] == 0
    print $ myLengthFunc [1, 2, 3] == 3

myLengthRecPM :: [Int] -> Int
myLengthRecPM [] = 0
myLengthRecPM (_:xs) = 1 + myLengthRecPM(xs)

myLengthRecNonPM :: [Int] -> Int
myLengthRecNonPM xs
 | null xs = 0
 | otherwise = 1 + (myLengthRecNonPM $ tail xs)

myLengthFunc :: [Int] -> Int
myLengthFunc = length