main :: IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+4)]) 1 == 7 -- my test

pairCompose :: [(Int -> Int)] -> (Int -> Int)
pairCompose [] = (\ x -> x) 
pairCompose [f] = (\ x -> f x)
pairCompose (f:g:funcs) = (\ x -> f $ g $ x + pairCompose funcs x)