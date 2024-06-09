main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
    -- my test 
    print $ convert myTree == Node 52 (Node 41 (Node 45 Nil Nil) (Node 31 Nil (Node 10 Nil Nil))) (Node 52 (Node 48 (Node 41 Nil Nil) (Node 25 Nil (Node 18 Nil Nil))) (Node 53 Nil Nil))



convert :: Tree -> Tree
convert tree = helper tree tree
 where
    helper :: Tree -> Tree -> Tree
    helper Nil _ = Nil
    helper (Node value left right) original = Node (getSum original value) (helper left original) (helper right original)


checkValue :: Int -> Int -> Int
checkValue value start
 | value >= start = value
 | otherwise = 0

getSum :: Tree -> Int -> Int
getSum Nil _ = 0
getSum (Node value Nil Nil) start = checkValue value start
getSum (Node value left right) start = checkValue value start + getSum left start + getSum right start



data Tree = Nil | Node Int Tree Tree
 deriving(Show, Eq)

tree :: Tree
tree = (Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil))))

myTree :: Tree
myTree = (Node 2 (Node 5 (Node 4 Nil Nil) (Node 6 Nil (Node 10 Nil Nil))) (Node 2 (Node 3 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil))) (Node 1 Nil Nil)))