main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2
    print $ minDepthGreenNode myTree == 1 -- my test


minDepthGreenNode :: Tree -> Int
minDepthGreenNode Empty = 0
minDepthGreenNode (Node Green _ _) = 1
minDepthGreenNode (Node _ left right) = 1 + min (minDepthGreenNode left) (minDepthGreenNode right)

data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

myTree :: Tree
myTree = Node Red(Node Blue (Node Blue (Node Red Empty Empty) (Node Red Empty Empty)) (Node Red (Node Red (Node Green Empty Empty) (Node Blue Empty Empty)) Empty)) Empty