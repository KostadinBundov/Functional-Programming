main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 4 [1, 2, 3, 4] == True -- my test

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    print $ isPresentRecPM 0 [1, 2, 3, 5] == False -- my test

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [1, 2, 3, 0, 4, 5, 6, 6, 7] == True -- my test

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc number = elem number

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM number (x:xs) = number == x || isPresentRecPM number xs

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM number xs = not (null xs) && (number == head xs || isPresentRecNonPM number (tail xs))
