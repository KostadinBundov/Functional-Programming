main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ specialSum 100 200 == 495 -- my test

specialSum :: Int -> Int -> Int
specialSum start end = sum [x | x <- [min start end .. max start end], contains x 6 && canBeExpressed x]

contains :: Int -> Int -> Bool
contains number digit = elem (head (show digit)) (show number)

canBeExpressed :: Int -> Bool
canBeExpressed number = any (\ x -> number == 4 * x + 1) [0 .. number]