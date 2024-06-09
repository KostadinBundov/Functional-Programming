main :: IO()
main = do
    print $ rangedSum firstTree 100 50 == 0 -- (L = 100, R = 50)
    print $ rangedSum firstTree 7 15 == 32 -- (L = 7, R = 15)
    print $ rangedSum firstTree 15 7 == 32 -- (L = 15, R = 7)
    print $ rangedSum secondTree 6 10 == 23 -- (L = 6, R = 10)
    print $ rangedSum secondTree 10 6 == 23 -- (L = 10, R = 6)
    print $ rangedSum myTree 1 20 == 34 -- my test



checkValue :: Int -> Int -> Int -> Int
checkValue value start end
 | value >= start && value <= end = value
 | otherwise = 0

rangedSum :: Tree -> Int -> Int -> Int
rangedSum tree start end = helper tree (min start end) (max start end)
 where
    helper Empty _ _ = 0
    helper (Node value Empty Empty) start end = checkValue value start end
    helper (Node value left right) start end = checkValue value start end + helper left start end + helper right start end

data Tree = Empty | Node Int Tree Tree

firstTree :: Tree
firstTree = (Node 10 (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) (Node 15 Empty (Node 18 Empty Empty)))

secondTree :: Tree
secondTree = (Node 10 (Node 5 (Node 3 (Node 1 Empty Empty) Empty) (Node 7 (Node 6 Empty Empty) Empty)) (Node 15 (Node 13 Empty Empty) (Node 18 Empty Empty)))

myTree :: Tree
myTree = (Node 10 (Node 12 Empty Empty) (Node 30 (Node 12 Empty Empty) (Node 100 Empty Empty)))