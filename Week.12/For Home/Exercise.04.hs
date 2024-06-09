import Data.List

main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False
    print $ leavesAreEqual t4 myTree == False -- my test
    print $ leavesAreEqual t4 myTree2 == True -- my test



leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual tree1 tree2 = sort (extractLeaves tree1) == sort (extractLeaves tree2)

extractLeaves :: BTree -> [Int]
extractLeaves Nil = []
extractLeaves (Node value Nil Nil) = [value]
extractLeaves (Node value left right) = extractLeaves left ++ extractLeaves right

data BTree = Nil | Node Int BTree BTree

t1 :: BTree
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: BTree
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t3 :: BTree
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t4 :: BTree
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

myTree :: BTree
myTree = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 (Node 13 (Node 27 Nil Nil) (Node 32 Nil Nil)) Nil)))

myTree2 :: BTree
myTree2 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 Nil (Node 32 (Node 13 (Node 27 Nil Nil) (Node 32 Nil Nil)) Nil)))