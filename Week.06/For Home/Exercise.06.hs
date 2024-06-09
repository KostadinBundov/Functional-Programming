main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [Integer -> Integer] -> (Integer -> Integer)
getOddCompositionValue functions = foldr (.) id filteredFunctions
  where
    indexedFunctions = zip [1..] functions
    oddIndexedFunctions = filter (\ (index, _) -> odd index) indexedFunctions
    filteredFunctions = map (\ (_, f) -> f) oddIndexedFunctions