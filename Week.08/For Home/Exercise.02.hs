import Data.List

main :: IO()

main = do
    -- you may get slightly different results eg. [3, 4, 5] on test 1 <- not a problem
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    print $ listLeaves [(1, 2, 3), (2, 3, 4), (3, 4, 1)] == [4] -- my test

type Node = Int
type Tree = [(Node, Node, Node)]

listLeaves :: Tree -> [Node]
listLeaves tree = [x | x <- nodes, notElem x parents]
  where 
    nodes = nub $ concatMap ( \(x, y, z) -> [x, y, z]) tree
    parents = map ( \(p, _, _) -> p) tree