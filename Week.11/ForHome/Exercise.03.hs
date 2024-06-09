main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2
    print $ maxDepthBlueNode myTree == 3 -- my test

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Empty = 0
maxDepthBlueNode (Node Blue left right) = 1 + max (maxDepthBlueNode left) (maxDepthBlueNode right)
maxDepthBlueNode (Node _ left right) = max (maxDepthBlueNode left) (maxDepthBlueNode right)

data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = (Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty))


myTree :: Tree
myTree = (Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red (Node Blue Empty Empty) Empty)) Empty))