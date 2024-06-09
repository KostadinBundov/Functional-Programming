main :: IO()
main = do
    print $ "Hello, world!"
    print $ t

data NTree a = Nil | Node a [NTree a]
 deriving (Show)

t :: NTree Int
t = Node 6 [Node 5 [Nil], Node 6 [Nil], Node 5 [Nil]]