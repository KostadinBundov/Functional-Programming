main :: IO ()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False
    print $ ordered t3 == False -- my test

isSubInterval :: (Int, Int) -> (Int, Int) -> Bool
isSubInterval (start, end) (mainStart, mainEnd) = start >= mainStart && end <= mainEnd

traverseDFS :: BTree -> [(Int, Int)]
traverseDFS Empty = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

getPairs :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
getPairs arr = zip arr (tail arr)

ordered :: BTree -> Bool
ordered = areSubIntervals . getPairs . traverseDFS 

areSubIntervals :: [((Int, Int), (Int, Int))] -> Bool
areSubIntervals = all (\((s1, e1), (s2, e2)) -> isSubInterval (s1, e1) (s2, e2))

data BTree = Empty | Node (Int, Int) BTree BTree

t1 :: BTree 
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Empty Empty) (Node (4, 9) Empty Empty)) (Node (2, 12) Empty (Node (1, 15) Empty Empty))

t2 :: BTree 
t2 = Node (3, 10) (Node (5, 8) (Node (6, 7) Empty Empty) (Node (7, 9) Empty Empty)) (Node (2, 12) Empty (Node (1, 15) Empty Empty))

t3 :: BTree 
t3 = Node (3, 10) (Node (5, 8) (Node (6, 7) Empty Empty) (Node (10, 15) Empty Empty)) (Node (2, 12) Empty (Node (1, 15) Empty Empty))