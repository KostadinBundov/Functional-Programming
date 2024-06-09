main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    print $ (repeater "Kostadin") 3 " " == "Kostadin Kostadin Kostadin" -- my test

repeater :: String -> (Int -> String -> String)
repeater str = (\count glue -> concat $ replicate (count - 1) (str ++ glue) ++ [str])