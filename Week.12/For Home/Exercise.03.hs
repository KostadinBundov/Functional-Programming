main :: IO()
main = do
    print $ levelSum numberTree 1 == 11 -- (5 + 6)
    print $ cone numberTree == True
    print $ levelSum myTree 2 == 25 -- my test
    print $ cone myTree == False -- my test



levelSum :: Tree -> Int -> Int
levelSum tree k = helper tree 0
 where
    helper :: Tree -> Int -> Int
    helper Nil _ = 0
    helper (Node value left right) currentLevel
      | currentLevel == k = value
      | otherwise = helper left (currentLevel + 1) + helper right (currentLevel + 1)


cone :: Tree -> Bool
cone Nil = True
cone (Node _ Nil Nil) = True
cone (Node _ left right) = cone left && cone right && levelSum left 1 <= levelSum right 1


data Tree = Nil | Node Int Tree Tree

numberTree :: Tree
numberTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

myTree :: Tree
myTree = Node 10 (Node 5 (Node 1 (Node 14 Nil Nil) Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil (Node 19 Nil Nil)) (Node 7 Nil Nil))